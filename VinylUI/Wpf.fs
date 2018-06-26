namespace VinylUI.Wpf

open System
open System.Collections
open System.Collections.Generic
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Data
open System.Runtime.CompilerServices
open Microsoft.FSharp.Quotations
open VinylUI

[<Extension>]
type WindowExtensions =
    [<Extension>]
    static member Show (window: 'Window, start: 'Window -> ISignal<_> * IDisposable) =
        let modelSignal, subscription = start window
        (window :> Window).Closed.Add (fun _ -> subscription.Dispose())
        window.Show()
        modelSignal

    [<Extension>]
    static member ShowDialog (window: 'Window, start: 'Window -> ISignal<_> * IDisposable) =
        let modelSignal, subscription = start window
        use x = subscription
        (window :> Window).ShowDialog() |> ignore
        modelSignal.Value

    [<Extension>]
    static member Run (app: Application, window: 'Window, start: 'Window -> ISignal<_> * IDisposable) =
        let modelSignal, subscription = start window
        use x = subscription
        app.Run(window) |> ignore
        modelSignal.Value

type FSharpValueConverter() =
    interface IValueConverter with
        member this.Convert (value, _, _, _) =
            if value = null then value
            else
                let t = value.GetType()
                if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
                    t.GetProperty("Value").GetValue(value)
                else value

        member this.ConvertBack (value, targetType, _, _) =
            if value = null then null
            else
                let optType =
                    if targetType.ContainsGenericParameters then
                        targetType.MakeGenericType([|value.GetType()|])
                    else targetType
                optType.GetConstructors().[0].Invoke([|value|])

[<AutoOpen>]
module ControlExtensions =
    let private addGridConverter (column: DataGridColumn) =
        match column with
        | :? DataGridTextColumn as c ->
            match c.Binding with
            | :? Data.Binding as b when b.Converter = null ->
                b.Converter <- FSharpValueConverter()
            | _ -> ()
        | _ -> ()

    type DataGrid with
        /// Adds binding converters to handle option types for the current columns and auto-generated columns.
        member this.AddFSharpConverterToColumns() =
            this.Columns |> Seq.iter addGridConverter
            this.AutoGeneratingColumn.Add <| fun e ->
                let t = e.PropertyType
                if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then
                    addGridConverter e.Column

module WpfBinding =
    open System.Reflection

    type WpfBindingMode = System.Windows.Data.BindingMode

    let getSourceUpdateMode updateMode =
        match updateMode with
        | OnValidation -> UpdateSourceTrigger.LostFocus
        | OnChange -> UpdateSourceTrigger.PropertyChanged

    let getUpdateModes bindingMode =
        let getSourceMode = function
            | Some m -> getSourceUpdateMode m
            | None -> UpdateSourceTrigger.Default
        match bindingMode with
        | TwoWay sm -> (WpfBindingMode.TwoWay, getSourceMode sm)
        | OneWayToModel sm -> (WpfBindingMode.OneWayToSource, getSourceMode sm)
        | OneWayToView -> (WpfBindingMode.OneWay, UpdateSourceTrigger.Explicit)

    let getDependencyProperty (property: PropertyInfo) =
        let depFieldName = property.Name + "Property"
        let isDepProp typ = typeof<DependencyProperty>.IsAssignableFrom(typ)
        let field = property.DeclaringType.GetField(depFieldName, BindingFlags.Public ||| BindingFlags.Static)
        if field <> null && isDepProp field.FieldType then
            field.GetValue(null) :?> DependencyProperty
        else
            let prop = property.DeclaringType.GetProperty(depFieldName, BindingFlags.Public ||| BindingFlags.Static)
            if prop <> null && isDepProp prop.PropertyType then
                prop.GetValue(null) :?> DependencyProperty
            else
                failwithf "Could not find dependency property %s on type %s" depFieldName property.DeclaringType.FullName

    let getConverter (bc: BindingConverter<'c, 's>) =
        { new IValueConverter with
          member this.Convert (value, _, _, _) = value |> unbox |> bc.ToControl |> box
          member this.ConvertBack (value, _, _, _) = value |> unbox |> bc.ToSource |> box
        }

    let createBinding (bi: BindingInfo<Control, 'c, 's>) =
        let mode, sourceUpdate = getUpdateModes bi.BindingMode
        let binding = Binding()
        binding.Source <- bi.Source
        binding.Path <- PropertyPath bi.SourceProperty.Name
        binding.Mode <- mode
        binding.UpdateSourceTrigger <- sourceUpdate
        bi.Converter |> Option.iter (getConverter >> binding.set_Converter)
        if mode = WpfBindingMode.OneWayToSource then
            // workaround for bug where one-way-to-source resets control value
            binding.FallbackValue <- bi.ControlProperty.GetValue bi.Control
        bi.Control.SetBinding(getDependencyProperty bi.ControlProperty, binding) |> ignore
        binding

    let bindControl bindingInfo = createBinding bindingInfo |> ignore

/// Functions for creating bindings
module Bind =
    /// Start the creation of a binding on a control property
    let view controlProperty = CommonBinding.controlPart<Control, 'View> controlProperty

/// Helpers for setting the ItemsSource of list controls
module ListSource =
    open VinylUI.BindingPatterns

    let private setSource (control: Selector) (source: 'a seq) valueMember displayMember =
        let setSelection =
            match control.SelectedValue with
            | null -> (fun () -> control.SelectedIndex <- -1)
            | s -> (fun () -> control.SelectedValue <- s)
        control.SelectedValuePath <- valueMember
        control.DisplayMemberPath <- displayMember
        control.ItemsSource <- Seq.toArray source
        setSelection ()

    /// Set the ItemsSource to a sequence of objects.
    /// `valueDisplayProperties` should be a quotation of a function that takes an item and returns a tuple of the
    /// value then display properties, e.g. <@ fun x -> x.Id, x.Name @>
    let fromSeq control (valueDisplayProperties: Expr<'a -> (_ * _)>) (source: 'a seq) =
        let (valueMember, displayMember) =
            match valueDisplayProperties with
            | PropertyTupleSelector (valueProp, displayProp) -> (valueProp.Name, displayProp.Name)
            | _ -> failwith (sprintf "Expected an expression that selects a tuple of the value property and display property, but was given %A" valueDisplayProperties)
        setSource control source valueMember displayMember

    /// Set the ItemsSource to the contents of a dictionary.
    /// The keys will be the control items' values and the dictionary values will be the items' text.
    let fromDict control (source: IDictionary<'value, 'display>) =
        setSource control source "Key" "Value"

    /// Set the ItemsSource to a sequence of value * display pairs.
    let fromPairs control (source: ('value * 'display) seq) = fromDict control (dict source)

[<Extension>]
type BindPartExtensions =
    /// Create a two-way binding between control and model properties of the same type.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay sourceUpdateMode)
        |> CommonBinding.createProxy WpfBinding.bindControl

    /// Create a two-way binding between control and model properties of different types given the conversions between them.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, _>, modelProperty, toModel, toView, ?sourceUpdateMode) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay sourceUpdateMode) with
            Converter = Some { ToControl = toView; ToSource = toModel }
        } |> CommonBinding.createProxy WpfBinding.bindControl

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
    static member toModel (view: BindViewPart<Control, obj>, modelProperty: Expr<_>, ?sourceUpdateMode) =
        let converter = BindingConverters.getObjConverter ()
        view.toModel(modelProperty, converter.ToSource, converter.ToControl, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a two-way binding, automatically converting between string and string option, where null and whitespace from the view becomes None on the model
    [<Extension>]
    static member toModel (view: BindViewPart<Control, string>, modelProperty: Expr<string option>, ?sourceUpdateMode) =
        let conv = BindingConverters.getStringConverter ()
        view.toModel(modelProperty, conv.ToSource, conv.ToControl, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a two-way binding between an IEnumerable control property and a model property.
    [<Extension>]
    static member toModel (view: BindViewPart<Control, IEnumerable>, modelProperty: Expr<'a seq>, ?sourceUpdateMode) =
        view.toModel(modelProperty, Seq.cast<'a>, (fun s -> upcast s), ?sourceUpdateMode = sourceUpdateMode)


    /// Create a one-way binding from a control property to a model property of the same type.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, 'a>, modelProperty: Expr<'a>, ?sourceUpdateMode) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel sourceUpdateMode)
        |> CommonBinding.createProxy WpfBinding.bindControl

    /// Create a one-way binding from a control property to a model property of a different type given the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, _>, modelProperty, toModel, ?sourceUpdateMode) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel sourceUpdateMode) with
            Converter = Some { ToControl = (fun _ -> failwith "one way binding"); ToSource = toModel }
        } |> CommonBinding.createProxy WpfBinding.bindControl

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
    static member toModelOneWay (view: BindViewPart<Control, obj>, modelProperty: Expr<_>, ?sourceUpdateMode) =
        let converter = BindingConverters.getObjConverter ()
        view.toModelOneWay(modelProperty, converter.ToSource, ?sourceUpdateMode = sourceUpdateMode)

    /// Create a one-way binding from an IEnumerable control property to a model property.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<Control, IEnumerable>, modelProperty: Expr<'a seq>, ?sourceUpdateMode) =
        view.toModelOneWay(modelProperty, Seq.cast<'a>, ?sourceUpdateMode = sourceUpdateMode)


    /// Create a one-way binding from a model property to a control property of the same type.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<'a>, viewProperty: Expr<'a>) =
        CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView
        |> CommonBinding.createProxy WpfBinding.bindControl

    /// Create a one-way binding from a model property to a control property of a different type given the conversion.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<_>, viewProperty, toView) =
        { CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView with
            Converter = Some { ToControl = toView; ToSource = (fun _ -> failwith "one way binding") }
        } |> CommonBinding.createProxy WpfBinding.bindControl

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
    static member toViewOneWay (source: BindSourcePart<_>, viewProperty: Expr<obj>) =
        let converter = BindingConverters.getObjConverter ()
        source.toViewOneWay(viewProperty, converter.ToControl)

    /// Create a one-way binding from a model property to an IEnumerable control property.
    [<Extension>]
    static member toViewOneWay (source: BindSourcePart<_>, viewProperty: Expr<IEnumerable>) =
        source.toViewOneWay(viewProperty, (fun s -> upcast s))


    /// Create a one-way binding from a model property of type 'a seq to the ItemsSource of a list control.
    /// `valueDisplayProperties` should be a quotation of a function that takes an 'a and returns a tuple of the
    /// value then display properties, e.g. <@ fun x -> x.Id, x.Name @>
    [<Extension>]
    static member toItemsSource (source: BindSourcePart<_>, control, valueDisplayProperties) =
        source.toFunc(ListSource.fromSeq control valueDisplayProperties)

    /// Create a one-way binding from an model property of type IDictionary<,> to the ItemsSource of a list control.
    [<Extension>]
    static member toItemsSource (source: BindSourcePart<IDictionary<'value, 'display>>, control) =
        source.toFunc(ListSource.fromDict control)

    /// Create a one-way binding from a model property of type ('a * 'b) seq to the ItemsSource of a list control.
    [<Extension>]
    static member toItemsSource (source: BindSourcePart<_>, control) =
        source.toFunc(ListSource.fromPairs control)
