namespace VinylUI.WinForms

open System
open System.Reflection
open System.Windows.Forms
open System.Runtime.CompilerServices
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open VinylUI

type WinBinding = System.Windows.Forms.Binding

module DataBind =
    let private convertUpdateMode = function
        | OnValidation -> DataSourceUpdateMode.OnValidation
        | OnChange -> DataSourceUpdateMode.OnPropertyChanged

    let private getUpdateModeFor (controlProperty: PropertyInfo) updateMode =
        match updateMode, controlProperty.Name with
        | OneWayToView, _ -> DataSourceUpdateMode.Never
        | OneWayToModel (Some mode), _
        | TwoWay (Some mode), _ -> convertUpdateMode mode
        | _, "Checked" -> DataSourceUpdateMode.OnPropertyChanged
        | _ -> DataSourceUpdateMode.OnValidation

    let private addConverter (binding: WinBinding) (converter: BindingConverter<'c, 's>) =
        let sanitize (ctrlValue: obj) =
            match ctrlValue with
            | :? DBNull -> Unchecked.defaultof<'c>
            | value -> value |> unbox
        binding.Format.Add (fun e -> e.Value <- e.Value |> unbox |> converter.ToControl)
        binding.Parse.Add (fun e -> e.Value <- e.Value |> sanitize |> converter.ToSource)

    let createBinding (bindingInfo: BindingInfo<Control, 'c, 's>) =
        let b = bindingInfo
        let sourceUpdate = getUpdateModeFor b.ControlProperty b.BindingMode
        let controlUpdate =
            match b.BindingMode with
            | OneWayToModel _ -> ControlUpdateMode.Never
            | _ -> ControlUpdateMode.OnPropertyChanged
        let controlBinding = WinBinding(b.ControlProperty.Name,
                                        b.Source,
                                        b.SourceProperty.Name,
                                        true,
                                        sourceUpdate,
                                        ControlUpdateMode = controlUpdate)
        b.Converter |> Option.iter (addConverter controlBinding)

        // workaround for bindings to SelectedItem not triggering until focus is lost
        if b.ControlProperty.Name = "SelectedItem" && sourceUpdate = DataSourceUpdateMode.OnPropertyChanged then
            let write _ = controlBinding.WriteValue()
            match b.Control with
            | :? ComboBox as c -> c.SelectedIndexChanged.Add write
            | :? ListBox as c -> c.SelectedIndexChanged.Add write
            | _ -> ()

        b.Control.DataBindings.Add controlBinding
        controlBinding


/// Helpers for setting the DataSource of ListControls
module ListSource =
    open VinylUI.BindingPatterns

    let private setSource (control: ListControl) (source: 'a seq) valueMember displayMember =
        control.DataSource <- null
        (* we have to set the members both before and after setting the DataSource. We have to set it before in case
         * event handlers try to read a value as soon as the DataSource is set, and we have to set it after because 
         * CheckedListBox and DataGridViewComboBoxEditingControl forget their DisplayMember when DataSource is set *)
        control.DisplayMember <- displayMember
        control.ValueMember <- valueMember
        control.DataSource <- Seq.toArray source
        control.DisplayMember <- displayMember
        control.ValueMember <- valueMember
        control.SelectedIndex <- -1

    /// Set the DataSource to a sequence of objects.
    /// `valueDisplayProperties` should be a quotation of a function that takes an item and returns a tuple of the
    /// value then display properties, e.g. <@ fun x -> x.Id, x.Name @>
    let fromSeq control (valueDisplayProperties: Expr<'a -> (_ * _)>) (source: 'a seq) =
        let (valueMember, displayMember) =
            match valueDisplayProperties with
            | PropertyTupleSelector (valueProp, displayProp) -> (valueProp.Name, displayProp.Name)
            | _ -> failwith (sprintf "Expected an expression that selects a tuple of the value property and display property, but was given %A" valueDisplayProperties)
        setSource control source valueMember displayMember

    /// Set the DataSource to the contents of a dictionary.
    /// The keys will be the control items' values and the dictionary values will be the items' text.
    let fromDict control (source: IDictionary<'value, 'display>) =
        setSource control source "Key" "Value"

    /// Set the DataSource to a sequence of value * display pairs.
    let fromPairs (control: ListControl) (source: ('value * 'display) seq) = fromDict control (dict source)


/// Functions for creating bindings
module Bind =
    /// Start the creation of a binding on a control property
    let view controlProperty = CommonBinding.controlPart<Control, 'View> controlProperty

    /// Start the creation of a binding on a model property
    let model modelProperty = CommonBinding.modelPart modelProperty

[<Extension>]
type BindPartExtensions =
    // used via reflection
    static member private _objToOptionVal () =
        { ToSource = (fun x -> x |> unbox |> Option.ofNullable)
          ToControl = Option.toNullable >> box }
    static member private _objToOptionRef () =
        { ToSource = (fun x -> if x = null then None else x |> unbox |> Option.ofObj)
          ToControl = Option.toObj >> box }

    static member private getObjConverter<'a> () =
        if typedefof<'a> = typedefof<option<_>> then
            let wrappedT = typeof<'a>.GetGenericArguments().[0]
            let kind = if wrappedT.IsValueType then "Val" else "Ref"
            typedefof<BindPartExtensions>.GetMethod("_objToOption" + kind, BindingFlags.Static ||| BindingFlags.NonPublic)
                                         .MakeGenericMethod([| wrappedT |])
                                         .Invoke(null, null) :?> BindingConverter<obj, 'a>
        else
            { ToSource = unbox
              ToControl = box }


    /// Create a two-way binding between control and model properties of the same type.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay sourceUpdateMode)
        |> CommonBinding.createProxy DataBind.createBinding

    /// Create a two-way binding between control and model properties of different types given the conversions between them.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, _>, modelProperty, toModel, toView, ?sourceUpdateMode) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay sourceUpdateMode) with
            Converter = Some { ToControl = toView; ToSource = toModel }
        } |> CommonBinding.createProxy DataBind.createBinding

    /// Create a two-way binding, automatically converting between option<'a> and 'a.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModel(modelProperty, Option.ofObj, Option.toObj, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a two-way binding, automatically converting between option<'a> and Nullable<'a>.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, Nullable<'a>>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModel(modelProperty, Option.ofNullable, Option.toNullable, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a two-way binding between an obj control property and a model property, automatically boxing and unboxing.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, obj>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        let converter = BindPartExtensions.getObjConverter<'a>()
        view.toModel(modelProperty, converter.ToSource, converter.ToControl, ?sourceUpdateMode = sourceUpdateMode)


    /// Create a one-way binding from a control property to a model property of the same type.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel sourceUpdateMode)
        |> CommonBinding.createProxy DataBind.createBinding

    /// Create a one-way binding from a control property to a model property of a different type given the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, _>, modelProperty, toModel, ?sourceUpdateMode) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel sourceUpdateMode) with
            Converter = Some { ToControl = (fun _ -> failwith "one way binding"); ToSource = toModel }
        } |> CommonBinding.createProxy DataBind.createBinding

    /// Create a one-way binding from a nullable reference control property to an option model property, automatically handling the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModelOneWay(modelProperty, Option.ofObj, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a one-way binding from a nullable control property to an option model property, automatically handling the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, Nullable<'a>>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModelOneWay(modelProperty, Option.ofNullable, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a one-way binding from an obj control property to a model property, automatically handling the unboxing.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, obj>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        let converter = BindPartExtensions.getObjConverter<'a>()
        view.toModelOneWay(modelProperty, converter.ToSource, ?sourceUpdateMode = sourceUpdateMode)


    /// Create a one-way binding from a model property to a control property of the same type.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a>, viewProperty: Expr<'a>) =
        CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView
        |> CommonBinding.createProxy DataBind.createBinding

    /// Create a one-way binding from a model property to a control property of a different type given the conversion.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<_>, viewProperty, toView) =
        { CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView with
            Converter = Some { ToControl = toView; ToSource = (fun _ -> failwith "one way binding") }
        } |> CommonBinding.createProxy DataBind.createBinding

    /// Create a one-way binding from a option model property to an nullable reference control property, automatically handling the conversion.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a option>, viewProperty: Expr<'a>) =
        source.toViewOneWay(viewProperty, Option.toObj)

    /// Create a one-way binding from a option model property to an nullable control property, automatically handling the conversion.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a option>, viewProperty: Expr<Nullable<'a>>) =
        source.toViewOneWay(viewProperty, Option.toNullable)

    /// Create a one-way binding from a model property to an obj control property, automatically handling the boxing.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a>, viewProperty: Expr<obj>) =
        let converter = BindPartExtensions.getObjConverter<'a>()
        source.toViewOneWay(viewProperty, converter.ToControl)


    /// Create a one-way binding from a model property to a function call that updates the view.
    [<Extension>]
    static member toFunc (source: BindSourcePart<'a>, updateView) =
        let update = unbox<'a> >> updateView
        update (source.SourceProperty.GetValue source.Source)
        { ModelProperty = source.SourceProperty
          ViewChanged = Event<_>().Publish
          SetView = update
        }

    /// Create a one-way binding from a model property to the DataSource of a ListControl.
    /// `valueDisplayProperties` should be a quotation of a function that takes an item and returns a tuple of the
    /// value then display properties, e.g. <@ fun x -> x.Id, x.Name @>
    [<Extension>]
    static member toDataSource (source: BindSourcePart<_>, control, valueDisplayProperties) =
        source.toFunc(ListSource.fromSeq control valueDisplayProperties)

    /// Create a one-way binding from a model property to the DataSource of a ListControl.
    [<Extension>]
    static member toDataSource (source: BindSourcePart<IDictionary<'value, 'display>>, control) =
        source.toFunc(ListSource.fromDict control)

    /// Create a one-way binding from a model property to the DataSource of a ListControl.
    [<Extension>]
    static member toDataSource (source: BindSourcePart<_>, control) =
        source.toFunc(ListSource.fromPairs control)

[<Extension>]
type FormExtensions =
    [<Extension>]
    static member Show (form: Form, (modelSignal: ISignal<_>, subscription: IDisposable)) =
        form.Closed.Add (fun _ -> subscription.Dispose())
        form.Show()
        modelSignal

    [<Extension>]
    static member ShowDialog (form: Form, (modelSignal: ISignal<_>, subscription: IDisposable)) =
        try
            form.ShowDialog() |> ignore
            modelSignal.Value
        finally subscription.Dispose()
