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
    SourceProperty: PropertyChain
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
    SourceProperty: PropertyChain
}

type BindSourceMulti<'Model> = {
    Source: obj
    SourceProperties: PropertyChain list
}

module BindingPatterns =
    open FSharp.Quotations.Patterns
    open FSharp.Quotations.Evaluator

    let (|PropertyExpression|_|) = function
        | PropertyGet (Some source, propInfo, []) ->
            Some (QuotationEvaluator.EvaluateUntyped source, propInfo)
        | _ -> None

    let rec private (|PropertyChainExpr|_|) = function
        | PropertyGet (Some source, propInfo, []) ->
            match source with
            | PropertyChainExpr (source, chain: PropertyChain) ->
                Some (source, chain.Append propInfo)
            | _ ->
                Some (source, PropertyChain [propInfo])
        | _ -> None

    let (|PropertyChainExpression|_|) = function
        | PropertyChainExpr (source, chain) -> Some (QuotationEvaluator.EvaluateUntyped source, chain)
        | _ -> None

    let rec private (|PropertyListExpression|_|) = function
        | PropertyChainExpr (_, chain) :: PropertyListExpression rest -> Some (chain :: rest)
        | [] -> Some []
        | _ -> None

    let (|PropertyTupleExpression|_|) expr =
        match expr with
        | NewTuple (PropertyChainExpression (source, prop1) :: PropertyListExpression props) ->
            Some (source, prop1 :: props)
        | _ -> None

    let (|PropertyTupleSelector|_|) = function
        | Lambda (_, NewTuple [PropertyGet (_, displayProp, []); PropertyGet (_, valueProp, [])]) ->
            Some (displayProp, valueProp)
        | _ -> None

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
        let proxyBindInfo = { bindInfo with Source = proxy; SourceProperty = PropertyChain [BindingProxy.Property] }
        createBinding proxyBindInfo
        let viewChanged =
            match bindInfo.BindingMode with
            | OneWayToView -> None
            | _ -> Some (proxy.ViewChanged :> IObservable<_>)
        { ModelProperties = [bindInfo.SourceProperty]
          ViewChanged = viewChanged
          SetView = proxy.SetView
        }

    let controlPart<'Control, 'View> (controlProperty: Expr<'View>) : BindViewPart<'Control, 'View> =
        match controlProperty with
        | PropertyExpression (ctl, ctlProp) when typedefof<'Control>.IsAssignableFrom(ctl.GetType()) ->
            { Control = ctl :?> 'Control; ControlProperty = ctlProp }
        | _ -> failwithf "Expected a property access expression of an object of type %s, but got invalid expression: %A"
                         typedefof<'Control>.Name controlProperty

    let modelPart (modelProperty: Expr<'Model>) : BindSourcePart<'Model> =
        match modelProperty with
        | PropertyChainExpression (src, srcProp) ->
            { Source = src; SourceProperty = srcProp }
        | _ -> failwithf "Expected a quoted property access, example <@ model.Property @>, but got invalid expression: %A"
                         modelProperty

    let modelMulti (modelProperties: Expr<'Model>) : BindSourceMulti<'Model> =
        match modelProperties with
        | PropertyTupleExpression (src, srcProps) ->
            { Source = src; SourceProperties = srcProps }
        | _ -> failwithf "Expected a quoted tuple of 2 or more properties, example <@ model.Property1, model.Property2 @>, but got invalid expression: %A"
                         modelProperties

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
                match bi.SourceProperty.Chain with
                | [prop] ->
                    source.PropertyChanged.Add <| fun e ->
                        if e.PropertyName = prop.Name then updateView ()
                | [] -> failwith "Source property must be specified for to-view binding"
                | _ -> failwith "Cannot use source property chain for to-view binding"
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

    let validationConvert onError toSource toView invalidValue (bindingInfo: BindingInfo<_,_,_>) =
        let convertToSource x =
            let converted = toSource x
            let error =
                match converted with
                | Ok _ -> None
                | Error e -> Some e
            onError error
            converted
        let convertToControl = toView |> Option.map (fun toView ->
            function
            | Ok x ->
                onError None
                toView x
            | Error e ->
                onError (Some e)
                invalidValue
                |> Option.bind (fun f -> f ())
                |> Option.defaultWith (fun () ->
                    bindingInfo.ControlProperty.GetValue bindingInfo.Control |> unbox
                )
            )
        { bindingInfo with
            ConvertToSource = Some convertToSource
            ConvertToControl = convertToControl
        }


/// Functions for creating bindings
module Bind =
    /// Start the creation of a binding on an INotifyPropertyChanged-enabled view component property.
    let viewInpc controlProperty = CommonBinding.controlPart<INotifyPropertyChanged, 'View> controlProperty

    /// Start the creation of a binding on a model property.
    let model modelProperty = CommonBinding.modelPart modelProperty

    /// Start the creation of a binding on multiple model properties given a quoted tuple of properties.
    let modelMulti modelProperties = CommonBinding.modelMulti modelProperties

[<Extension>]
type BindPartExtensions =
    // two way

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

    /// Create a two-way binding between control and model properties of different types with validation.
    [<Extension>]
    static member toModelResult (view: BindViewPart<INotifyPropertyChanged, _>, modelProperty, toModelValidator, toView, onError) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (TwoWay None)
        |> CommonBinding.validationConvert onError toModelValidator (Some toView) None
        |> CommonBinding.createProxy CommonBinding.bindInpc

    /// Create a two-way binding between control and model properties of the same type with validation.
    [<Extension>]
    static member toModelResult (view: BindViewPart<INotifyPropertyChanged, _>, modelProperty, toModelValidator, onError) =
        view.toModelResult(modelProperty, toModelValidator, id, onError)

    // one way to model

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

    /// Create a one-way binding from a control property to a model property of a different type with validation.
    [<Extension>]
    static member toModelResultOneWay (view: BindViewPart<INotifyPropertyChanged, _>, modelProperty, toModelValidator, onError) =
        CommonBinding.fromParts view (CommonBinding.modelPart modelProperty) (OneWayToModel None)
        |> CommonBinding.validationConvert onError toModelValidator None None
        |> CommonBinding.createProxy CommonBinding.bindInpc

    // one way to view

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

    // model to callback

    /// Create a one-way binding from a model property to a function call that updates the view.
    [<Extension>]
    static member toFunc (source: BindSourcePart<'a>, updateView) =
        let update = unbox<'a> >> updateView
        update (source.SourceProperty.GetValue source.Source)
        { ModelProperties = [source.SourceProperty]
          ViewChanged = None
          SetView = update
        }

    [<Extension>]
    static member toFunc (source: BindSourceMulti<'a>, updateView) =
        updateView (Model.getTupledValues source.Source source.SourceProperties |> unbox)
        { ModelProperties = source.SourceProperties
          ViewChanged = None
          SetView = unbox<'a> >> updateView
        }
