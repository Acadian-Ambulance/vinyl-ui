namespace VinylUI

open System
open System.ComponentModel
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.Quotations

type SourceUpdateMode =
    | OnValidation
    | OnChange

type BindingMode =
    | TwoWay of SourceUpdateMode option
    | OneWayToModel of SourceUpdateMode option
    | OneWayToView

type BindingInfo<'Control, 'ControlProp, 'SourceProp> = {
    Control: 'Control
    ControlProperty: PropertyInfo
    Source: obj
    SourceProperty: PropertyInfo
    BindingMode: BindingMode
    ConvertToSource: ('ControlProp -> 'SourceProp) option
    ConvertToControl: ('SourceProp -> 'ControlProp) option
}

type BindViewPart<'Control, 'View> = {
    Control: 'Control
    ControlProperty: PropertyInfo
}

type BindSourcePart<'Model> = {
    Source: obj
    SourceProperty: PropertyInfo
}

module BindingPatterns =
    open FSharp.Quotations.Patterns
    open FSharp.Quotations.Evaluator

    let (|PropertyExpression|_|) = function
        | PropertyGet (Some source, propInfo, []) ->
            Some (QuotationEvaluator.EvaluateUntyped source, propInfo)
        | _ -> None

    let (|PropertyTupleSelector|_|) = function
        | Lambda (_, NewTuple [PropertyGet (_, displayProp, []); PropertyGet (_, valueProp, [])]) ->
            Some (displayProp, valueProp)
        | _ -> None

type Binding = {
    ModelProperty: PropertyInfo
    ViewChanged: IObservable<obj>
    SetView: obj -> unit
}

type BindingProxy(initValue) =
    let mutable value = initValue
    let modelChanged = Event<_,_>()
    let viewChanged = Event<obj>()

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = modelChanged.Publish
    member this.ViewChanged = viewChanged.Publish

    /// Used for databinding to the view
    member this.Value
        with get () = value
        and set v =
            if value <> v then
                value <- v
                viewChanged.Trigger v

    member this.SetView v =
        value <- v
        modelChanged.Trigger(this, PropertyChangedEventArgs("Value"))

    static member Property = typedefof<BindingProxy>.GetProperty("Value")

type BindingConvert() =
    // used via reflection
    static member private _objToOptionVal () : obj -> _ = unbox >> Option.ofNullable
    static member private _objFromOptionVal () = Option.toNullable >> box
    static member private _objToOptionRef () : obj -> _ = Option.ofObj >> Option.map unbox
    static member private _objFromOptionRef () = Option.toObj >> box

    static member private objConverter<'a> isFrom =
        let wrappedT = typeof<'a>.GetGenericArguments().[0]
        let direction = if isFrom then "From" else "To"
        let kind = if wrappedT.IsValueType then "Val" else "Ref"
        let name = sprintf "_obj%sOption%s" direction kind
        typedefof<BindingConvert>
            .GetMethod(name, BindingFlags.Static ||| BindingFlags.NonPublic)
            .MakeGenericMethod([| wrappedT |])
            .Invoke(null, null)

    static member objToOption<'a> () =
        if typedefof<'a> = typedefof<option<_>> then
            BindingConvert.objConverter<'a> false |> unbox<obj -> 'a>
        else unbox

    static member objFromOption<'a> () =
        if typedefof<'a> = typedefof<option<_>> then
            BindingConvert.objConverter<'a> true |> unbox<'a -> obj>
        else box

    static member toStringOption s = if String.IsNullOrWhiteSpace s then None else Some s
    static member fromStringOption s = s |> Option.defaultValue ""

module CommonBinding =
    open BindingPatterns

    let createProxy createBinding (bindInfo: BindingInfo<_, _, _>) =
        let value = bindInfo.SourceProperty.GetValue bindInfo.Source
        let proxy = BindingProxy value
        let proxyBindInfo = { bindInfo with Source = proxy; SourceProperty = BindingProxy.Property }
        createBinding proxyBindInfo
        { ModelProperty = bindInfo.SourceProperty
          ViewChanged = proxy.ViewChanged
          SetView = proxy.SetView
        }

    let controlPart<'Control, 'View> (controlProperty: Expr<'View>) =
        match controlProperty with
        | PropertyExpression (ctl, ctlProp) when typedefof<'Control>.IsAssignableFrom(ctl.GetType()) ->
            { Control = ctl :?> 'Control; ControlProperty = ctlProp } : BindViewPart<'Control, 'View>
        | _ -> failwithf "Expected a property access expression of an object of type %s" typedefof<'Control>.Name

    let modelPart (modelProperty: Expr<'Model>) =
        match modelProperty with
        | PropertyExpression (src, srcProp) ->
            { Source = src; SourceProperty = srcProp } : BindSourcePart<'Model>
        | _ -> failwith "Expected a property access expression"

    let fromParts (view: BindViewPart<'Control, 'View>) (source: BindSourcePart<'Source>) mode : BindingInfo<'Control, 'View, 'Source> =
        { Control = view.Control
          ControlProperty = view.ControlProperty
          Source = source.Source
          SourceProperty = source.SourceProperty
          BindingMode = mode
          ConvertToControl = None
          ConvertToSource = None
        }

    let bindInpc (bi: BindingInfo<INotifyPropertyChanged, 'c, 's>) =
        let updateModel () =
            let value = bi.ControlProperty.GetValue bi.Control :?> 'c
            let converted =
                match bi.ConvertToSource with
                | Some c -> c value
                | None -> box value :?> 's
            bi.SourceProperty.SetValue(bi.Source, converted)

        let updateView () =
            let value = bi.SourceProperty.GetValue bi.Source :?> 's
            let converted =
                match bi.ConvertToControl with
                | Some c -> c value
                | None -> box value :?> 'c
            bi.ControlProperty.SetValue(bi.Control, converted)

        let updateModelOnViewChange () =
            bi.Control.PropertyChanged.Add <| fun e ->
                if e.PropertyName = bi.ControlProperty.Name then updateModel ()

        let updateViewOnModelChange () =
            match bi.Source with
            | :? INotifyPropertyChanged as source ->
                source.PropertyChanged.Add <| fun e ->
                    if e.PropertyName = bi.SourceProperty.Name then updateView ()
            | _ -> failwith "Source must implement INotifyPropertyChanged for to-view binding"

        match bi.BindingMode with
        | TwoWay _ ->
            updateView()
            updateModelOnViewChange ()
            updateViewOnModelChange ()
        | OneWayToModel _ ->
            updateModel ()
            updateModelOnViewChange ()
        | OneWayToView _ ->
            updateView()
            updateViewOnModelChange ()

/// Functions for creating bindings
module Bind =
    /// Start the creation of a binding on an INotifyPropertyChanged-enabled view component property
    let viewInpc controlProperty = CommonBinding.controlPart<INotifyPropertyChanged, 'View> controlProperty

    /// Start the creation of a binding on a model property
    let model modelProperty = CommonBinding.modelPart modelProperty

[<Extension>]
type BindPartExtensions =
    /// Create a two-way binding between control and model properties of the same type.
    [<Extension>]
    static member toModel (view: BindViewPart<INotifyPropertyChanged, 'a>, modelProperty: Expr<'a>) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay None)
        |> CommonBinding.createProxy CommonBinding.bindInpc

    /// Create a two-way binding between control and model properties of different types given the conversions between them.
    [<Extension>]
    static member toModel (view: BindViewPart<INotifyPropertyChanged, _>, modelProperty, toModel, toView) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay None) with
            ConvertToSource = Some toModel
            ConvertToControl = Some toView
        } |> CommonBinding.createProxy CommonBinding.bindInpc

    /// Create a two-way binding, automatically converting between option<'a> and 'a.
    [<Extension>]
    static member toModel (view: BindViewPart<INotifyPropertyChanged, 'a>, modelProperty: Expr<'a option>) =
        view.toModel(modelProperty, Option.ofObj, Option.toObj)

    /// Create a two-way binding, automatically converting between option<'a> and Nullable<'a>.
    [<Extension>]
    static member toModel (view: BindViewPart<INotifyPropertyChanged, Nullable<'a>>, modelProperty: Expr<'a option>) =
        view.toModel(modelProperty, Option.ofNullable, Option.toNullable)

    /// Create a two-way binding, automatically converting between string and string option, where null and whitespace from the view becomes None on the model
    [<Extension>]
    static member toModel (view: BindViewPart<INotifyPropertyChanged, string>, modelProperty: Expr<string option>) =
        view.toModel(modelProperty, BindingConvert.toStringOption, BindingConvert.fromStringOption)


    /// Create a one-way binding from a control property to a model property of the same type.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<INotifyPropertyChanged, 'a>, modelProperty: Expr<'a>) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel None)
        |> CommonBinding.createProxy CommonBinding.bindInpc

    /// Create a one-way binding from a control property to a model property of a different type given the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<INotifyPropertyChanged, _>, modelProperty, toModel) =
        { CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel None) with
            ConvertToSource = Some toModel
        } |> CommonBinding.createProxy CommonBinding.bindInpc

    /// Create a one-way binding from a nullable reference control property to an option model property, automatically handling the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<INotifyPropertyChanged, 'a>, modelProperty: Expr<'a option>) =
        view.toModelOneWay(modelProperty, Option.ofObj)

    /// Create a one-way binding from a nullable control property to an option model property, automatically handling the conversion.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<INotifyPropertyChanged, Nullable<'a>>, modelProperty: Expr<'a option>) =
        view.toModelOneWay(modelProperty, Option.ofNullable)

    /// Create a one-way binding, from a string control property to a string option model property, 
    /// automatically handling the conversion where null and whitespace from the view becomes None on the model.
    [<Extension>]
    static member toModelOneWay (view: BindViewPart<INotifyPropertyChanged, string>, modelProperty: Expr<string option>) =
        view.toModelOneWay(modelProperty, BindingConvert.toStringOption)


    /// Create a one-way binding from a model property to a control property of the same type.
    [<Extension>]
    static member toViewInpcOneWay (source: BindSourcePart<'a>, viewProperty: Expr<'a>) =
        CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView
        |> CommonBinding.createProxy CommonBinding.bindInpc

    /// Create a one-way binding from a model property to a control property of a different type given the conversion.
    [<Extension>]
    static member toViewInpcOneWay (source: BindSourcePart<_>, viewProperty, toView) =
        { CommonBinding.fromParts (CommonBinding.controlPart viewProperty) source OneWayToView with
            ConvertToControl = Some toView
        } |> CommonBinding.createProxy CommonBinding.bindInpc

    /// Create a one-way binding from a option model property to an nullable reference control property, automatically handling the conversion.
    [<Extension>]
    static member toViewInpcOneWay (source: BindSourcePart<'a option>, viewProperty: Expr<'a>) =
        source.toViewInpcOneWay(viewProperty, Option.toObj)

    /// Create a one-way binding from a option model property to an nullable control property, automatically handling the conversion.
    [<Extension>]
    static member toViewInpcOneWay (source: BindSourcePart<'a option>, viewProperty: Expr<Nullable<'a>>) =
        source.toViewInpcOneWay(viewProperty, Option.toNullable)

    /// Create a one-way binding, from a string option model property to a string control property, 
    /// automatically handling the conversion where None on the model becomes empty string on the view.
    [<Extension>]
    static member toViewInpcOneWay (source: BindSourcePart<string option>, viewProperty: Expr<string>) =
        source.toViewInpcOneWay(viewProperty, BindingConvert.fromStringOption)


    /// Create a one-way binding from a model property to a function call that updates the view.
    [<Extension>]
    static member toFunc (source: BindSourcePart<'a>, updateView) =
        let update = unbox<'a> >> updateView
        update (source.SourceProperty.GetValue source.Source)
        { ModelProperty = source.SourceProperty
          ViewChanged = Event<_>().Publish
          SetView = update
        }
