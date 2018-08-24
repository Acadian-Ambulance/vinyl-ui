namespace VinylUI.WinForms

open System
open System.Windows.Forms
open System.Runtime.CompilerServices
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open VinylUI

module WinFormsBinding =
    type WinBinding = System.Windows.Forms.Binding

    let getSourceUpdateMode updateMode =
        match updateMode with
        | OnValidation -> DataSourceUpdateMode.OnValidation
        | OnChange -> DataSourceUpdateMode.OnPropertyChanged

    let getUpdateModes bindingMode =
        match bindingMode with
        | TwoWay sm -> (ControlUpdateMode.OnPropertyChanged, sm |> Option.map getSourceUpdateMode)
        | OneWayToModel sm -> (ControlUpdateMode.Never, sm |> Option.map getSourceUpdateMode)
        | OneWayToView -> (ControlUpdateMode.OnPropertyChanged, Some DataSourceUpdateMode.Never)

    let addConverter (binding: WinBinding) (converter: BindingConverter<'c, 's>) =
        let sanitize (ctrlValue: obj) =
            match ctrlValue with
            | :? DBNull -> Unchecked.defaultof<'c>
            | value -> value |> unbox
        binding.Format.Add (fun e -> e.Value <- e.Value |> unbox |> converter.ToControl)
        binding.Parse.Add (fun e -> e.Value <- e.Value |> sanitize |> converter.ToSource)

    let createBinding (bindingInfo: BindingInfo<Control, 'c, 's>) =
        let b = bindingInfo
        let controlUpdate, sourceUpdate = getUpdateModes b.BindingMode
        let sourceUpdate = sourceUpdate |> Option.defaultWith (fun () ->
            match b.ControlProperty.Name with
            | "Checked" -> DataSourceUpdateMode.OnPropertyChanged
            | _ -> DataSourceUpdateMode.OnValidation )
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
        
        // workaround for SelectedIndex needing to be set to -1 twice issue
        if b.ControlProperty.Name = "SelectedIndex" then
            let fixIndex (c: ListControl) =
                let sourceIndex = b.SourceProperty.GetValue(b.Source) :?> int
                if sourceIndex = -1 && c.SelectedIndex <> -1 then
                    c.SelectedIndex <- -1
            match b.Control with
            | :? ComboBox as c -> controlBinding.BindingComplete.Add (fun _ -> fixIndex c)
            | :? ListBox as c -> controlBinding.BindingComplete.Add (fun _ -> fixIndex c)
            | _ -> ()
        
        if b.ControlProperty.Name = "SelectedItem" then
            match b.Control with
            | :? ComboBox as c -> controlBinding.BindingComplete.Add <| fun _ -> 
                let sourceItem = b.SourceProperty.GetValue(b.Source)
                if sourceItem = null && c.SelectedItem <> null then
                    c.SelectedIndex <- -1
                    if c.SelectedIndex <> -1 then
                        c.SelectedIndex <- -1
            | _ -> ()

        if b.ControlProperty.Name = "SelectedValue" then
            let fixValue (c: ListControl) =
                let sourceItem = b.SourceProperty.GetValue(b.Source)
                if sourceItem = null && c.SelectedValue <> null then
                    c.SelectedIndex <- -1
            match b.Control with
            | :? ComboBox as c -> controlBinding.BindingComplete.Add (fun _ -> fixValue c)
            | :? ListBox as c -> controlBinding.BindingComplete.Add (fun _ -> fixValue c)
            | _ -> ()

        b.Control.DataBindings.Add controlBinding
        controlBinding

    let bindControl bi = createBinding bi |> ignore


/// Helpers for setting the DataSource of ListControls
module ListSource =
    open VinylUI.BindingPatterns

    let private setSource (control: ListControl) (source: 'a seq) valueMember displayMember =
        let setSelection =
            match control with
            | :? ListBox as c when c.SelectionMode = SelectionMode.None -> ignore
            | c when c.SelectedValue = null -> (fun () -> control.SelectedIndex <- -1)
            | c -> (fun () -> control.SelectedValue <- c.SelectedValue)
        control.DataSource <- null
        (* we have to set the members both before and after setting the DataSource. We have to set it before in case
         * event handlers try to read a value as soon as the DataSource is set, and we have to set it after because 
         * CheckedListBox and DataGridViewComboBoxEditingControl forget their DisplayMember when DataSource is set *)
        control.DisplayMember <- displayMember
        control.ValueMember <- valueMember
        control.DataSource <- Seq.toArray source
        control.DisplayMember <- displayMember
        control.ValueMember <- valueMember
        setSelection ()

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

[<Extension>]
type BindPartExtensions =
    /// Create a two-way binding between control and model properties of the same type.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay sourceUpdateMode)
        |> CommonBinding.createProxy WinFormsBinding.bindControl

    /// Create a two-way binding between control and model properties of different types given the conversions between them.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, _>, modelProperty, toModel, toView, ?sourceUpdateMode) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay sourceUpdateMode) with
            Converter = Some { ToControl = toView; ToSource = toModel }
        } |> CommonBinding.createProxy WinFormsBinding.bindControl

    /// Create a two-way binding, automatically converting between option<'a> and 'a.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModel(modelProperty, Option.ofObj, Option.toObj, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a two-way binding, automatically converting between option<'a> and Nullable<'a>.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, Nullable<'a>>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModel(modelProperty, Option.ofNullable, Option.toNullable, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a two-way binding, automatically converting between string and string option, where null and whitespace from the view becomes None on the model
    [<Extension>]
    static member toModel (view: BindViewPart<Control, string>, modelProperty: Expr<string option>, ?sourceUpdateMode) =
        let conv = BindingConverters.getStringConverter ()
        view.toModel(modelProperty, conv.ToSource, conv.ToControl, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a two-way binding between an obj control property and a model property, automatically boxing and unboxing.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, obj>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        let converter = BindingConverters.getObjConverter ()
        view.toModel(modelProperty, converter.ToSource, converter.ToControl, ?sourceUpdateMode = sourceUpdateMode)


    /// Create a one-way binding from a control property to a model property of the same type.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel sourceUpdateMode)
        |> CommonBinding.createProxy WinFormsBinding.bindControl

    /// Create a one-way binding from a control property to a model property of a different type given the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, _>, modelProperty, toModel, ?sourceUpdateMode) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel sourceUpdateMode) with
            Converter = Some { ToControl = (fun _ -> failwith "one way binding"); ToSource = toModel }
        } |> CommonBinding.createProxy WinFormsBinding.bindControl

    /// Create a one-way binding from a nullable reference control property to an option model property, automatically handling the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModelOneWay(modelProperty, Option.ofObj, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a one-way binding from a nullable control property to an option model property, automatically handling the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, Nullable<'a>>, modelProperty: Expr<'a option>, ?sourceUpdateMode) =
        view.toModelOneWay(modelProperty, Option.ofNullable, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a one-way binding, from a string control property to a string option model property, 
    /// automatically handling the conversion where null and whitespace from the view becomes None on the model.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, string>, modelProperty: Expr<string option>, ?sourceUpdateMode) =
        view.toModelOneWay(modelProperty, BindingConverters.getStringConverter().ToSource, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a one-way binding from an obj control property to a model property, automatically handling the unboxing.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, obj>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        let converter = BindingConverters.getObjConverter ()
        view.toModelOneWay(modelProperty, converter.ToSource, ?sourceUpdateMode = sourceUpdateMode)


    /// Create a one-way binding from a model property to a control property of the same type.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a>, viewProperty: Expr<'a>) =
        CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView
        |> CommonBinding.createProxy WinFormsBinding.bindControl

    /// Create a one-way binding from a model property to a control property of a different type given the conversion.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<_>, viewProperty, toView) =
        { CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView with
            Converter = Some { ToControl = toView; ToSource = (fun _ -> failwith "one way binding") }
        } |> CommonBinding.createProxy WinFormsBinding.bindControl

    /// Create a one-way binding from a option model property to an nullable reference control property, automatically handling the conversion.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a option>, viewProperty: Expr<'a>) =
        source.toViewOneWay(viewProperty, Option.toObj)

    /// Create a one-way binding from a option model property to an nullable control property, automatically handling the conversion.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a option>, viewProperty: Expr<Nullable<'a>>) =
        source.toViewOneWay(viewProperty, Option.toNullable)

    /// Create a one-way binding, from a string option model property to a string control property, 
    /// automatically handling the conversion where None on the model becomes empty string on the view.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<string option>, viewProperty: Expr<string>) =
        source.toViewOneWay(viewProperty, BindingConverters.getStringConverter().ToControl)

    /// Create a one-way binding from a model property to an obj control property, automatically handling the boxing.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a>, viewProperty: Expr<obj>) =
        let converter = BindingConverters.getObjConverter ()
        source.toViewOneWay(viewProperty, converter.ToControl)


    /// Create a one-way binding from a model property of type 'a seq to the DataSource of a ListControl.
    /// `valueDisplayProperties` should be a quotation of a function that takes an 'a and returns a tuple of the
    /// value then display properties, e.g. <@ fun x -> x.Id, x.Name @>
    [<Extension>]
    static member toDataSource (source: BindSourcePart<_>, control, valueDisplayProperties) =
        source.toFunc(ListSource.fromSeq control valueDisplayProperties)

    /// Create a one-way binding from an model property of type IDictionary<,> to the DataSource of a ListControl.
    [<Extension>]
    static member toDataSource (source: BindSourcePart<IDictionary<'value, 'display>>, control) =
        source.toFunc(ListSource.fromDict control)

    /// Create a one-way binding from a model property of type ('a * 'b) seq to the DataSource of a ListControl.
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
