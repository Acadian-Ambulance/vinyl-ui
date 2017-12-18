namespace VinylUI.WinForms

open System
open System.Reflection
open System.Windows.Forms
open System.Runtime.CompilerServices
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
            | value -> value :?> 'c
        binding.Format.Add (fun e -> e.Value <- e.Value :?> 's |> converter.ToControl)
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

        match b.Control, b.ControlProperty.Name, sourceUpdate with
        // workaround for bindings to SelectedItem not triggering until focus is lost
        | :? ComboBox as c, "SelectedItem", DataSourceUpdateMode.OnPropertyChanged ->
            c.SelectedIndexChanged.Add (fun _ -> controlBinding.WriteValue())
        | :? ListBox as c, "SelectedItem", DataSourceUpdateMode.OnPropertyChanged ->
            c.SelectedIndexChanged.Add (fun _ -> controlBinding.WriteValue())
        | _ -> ()

        b.Control.DataBindings.Add controlBinding
        controlBinding

    /// Creates bindings from a quotation of statements, where each statement is an assignment to
    /// a view property from a model property or a call to a function passing a model property.
    let fromExpr assignExprs = CommonBinding.fromExpr createBinding assignExprs

    /// temporary helper
    let fromProp source (property: PropertyInfo) (updateView: 'a -> unit) =
        updateView (property.GetValue source :?> 'a)
        let binding = {
            ModelProperty = property
            ViewChanged = Event<_>().Publish
            SetView = (fun o -> updateView (o :?> 'a))
        }
        binding


module Bind =
    /// Start the creation of a binding on a control property
    let view controlProperty = CommonBinding.controlPart<Control, 'View> controlProperty

    /// Start the creation of a binding on a model property
    let model modelProperty = CommonBinding.modelPart modelProperty

[<Extension; AutoOpen>]
type BindPartExtensions =
    // used via reflection
    static member private _objToOptionVal () =
        { ToSource = unbox<Nullable<'a>> >> Option.ofNullable
          ToControl = Option.toNullable >> box }
    static member private _objToOptionRef () =
        { ToSource = unbox<'a> >> Option.ofObj
          ToControl = Option.toObj >> box }

    static member private getObjConverter<'a> () =
        if typedefof<'a> = typedefof<option<_>> then
            let wrappedT = typeof<'a>.GetGenericArguments().[0]
            let kind = if wrappedT.IsValueType then "Val" else "Ref"
            typedefof<BindPartExtensions>.GetMethod("_objToOption" + kind, BindingFlags.Static ||| BindingFlags.NonPublic)
                                         .MakeGenericMethod([| wrappedT |])
                                         .Invoke(null, null) :?> BindingConverter<obj, 'a>
        else
            { ToSource = unbox<'a>
              ToControl = box }


    [<Extension>]
    static member toModel (view: BindViewPart<Control, 'View>, modelProperty: Expr<'View>, ?sourceUpdateMode) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay sourceUpdateMode)
        |> CommonBinding.createProxy DataBind.createBinding

    [<Extension>]
    static member toModel (view: BindViewPart<Control, 'View>, modelProperty: Expr<'Model>, toModel: 'View -> 'Model, toView: 'Model -> 'View, ?sourceUpdateMode) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay sourceUpdateMode) with
            Converter = Some { ToControl = toView; ToSource = toModel }
        } |> CommonBinding.createProxy DataBind.createBinding

    [<Extension>]
    static member toModel (view: BindViewPart<Control, Nullable<'a>>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModel(modelProperty, Option.ofNullable, Option.toNullable, ?sourceUpdateMode = sourceUpdateMode)

    [<Extension>]
    static member toModel (view: BindViewPart<Control, obj>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        let converter = BindPartExtensions.getObjConverter<'a>()
        view.toModel(modelProperty, converter.ToSource, converter.ToControl, ?sourceUpdateMode = sourceUpdateMode)


    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, 'View>, modelProperty: Expr<'View>, ?sourceUpdateMode) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel sourceUpdateMode)
        |> CommonBinding.createProxy DataBind.createBinding

    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, 'View>, modelProperty: Expr<'Model>, toModel: 'View -> 'Model, ?sourceUpdateMode) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel sourceUpdateMode) with
            Converter = Some { ToControl = (fun _ -> failwith "one way binding"); ToSource = toModel }
        } |> CommonBinding.createProxy DataBind.createBinding

    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, Nullable<'a>>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModelOneWay(modelProperty, Option.ofNullable, ?sourceUpdateMode = sourceUpdateMode)

    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, obj>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        let converter = BindPartExtensions.getObjConverter<'a>()
        view.toModelOneWay(modelProperty, converter.ToSource, ?sourceUpdateMode = sourceUpdateMode)


    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'Model>, viewProperty: Expr<'Model>) =
        CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView
        |> CommonBinding.createProxy DataBind.createBinding

    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'Model>, viewProperty: Expr<'View>, toView: 'Model -> 'View) =
        { CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView with
            Converter = Some { ToControl = toView; ToSource = (fun _ -> failwith "one way binding") }
        } |> CommonBinding.createProxy DataBind.createBinding

    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a option>, viewProperty: Expr<Nullable<'a>>) =
        source.toViewOneWay(viewProperty, Option.toNullable)

    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a>, viewProperty: Expr<obj>) =
        let converter = BindPartExtensions.getObjConverter<'a>()
        source.toViewOneWay(viewProperty, converter.ToControl)


    [<Extension>]
    static member toFunc (source: BindSourcePart<'Model>, updateView: 'Model -> unit) =
        let update = unbox<'Model> >> updateView
        update (source.SourceProperty.GetValue source.Source)
        { ModelProperty = source.SourceProperty
          ViewChanged = Event<_>().Publish
          SetView = update
        }


/// Helpers for setting the DataSource of ListControls
module ListSource =
    open System.Collections.Generic
    open Microsoft.FSharp.Quotations
    open VinylUI.BindingPatterns

    let private setSource (control: ListControl) (source: 'a seq) displayMember valueMember =
        control.DataSource <- null
        (* we have to set the members both before and after setting the DataSource. We have to set it before in case
         * event handlers try to read a value as soon as the DataSource is set, and we have to set it after because 
         * CheckedListBox and DataGridViewComboBoxEditingControl forget their DisplayMember when DataSource is set *)
        control.DisplayMember <- displayMember
        control.ValueMember <- valueMember
        control.DataSource <- List source
        control.DisplayMember <- displayMember
        control.ValueMember <- valueMember
        control.SelectedIndex <- -1

    // TODO: move these to BindSourcePart overloads `toSource`

    let fromSeq control (displayValueProperties: Expr<'a -> (_ * _)>) (source: 'a seq) =
        let (displayMember, valueMember) =
            match displayValueProperties with
            | PropertyTupleSelector (displayProp, valueProp) -> (displayProp.Name, valueProp.Name)
            | _ -> failwith (sprintf "Expected an expression that selects a tuple of the display property and value property, but was given %A" displayValueProperties)
        setSource control source displayMember valueMember

    /// Set the DataSource to the contents of a dictionary.
    /// The keys will be the control items' values and the dictionary values will be the items' text.
    let fromDict control (source: IDictionary<'value, 'display>) =
        setSource control source "Value" "Key"

    /// Set the DataSource to a sequence of value * display pairs.
    let fromPairs (control: ListControl) (source: ('value * 'display) seq) = fromDict control (dict source)
