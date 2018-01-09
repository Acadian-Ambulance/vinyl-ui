namespace VinylUI

open System
open System.ComponentModel
open System.Reflection
open FSharp.Quotations

type SourceUpdateMode =
    | OnValidation
    | OnChange

type BindingMode =
    | TwoWay of SourceUpdateMode option
    | OneWayToModel of SourceUpdateMode option
    | OneWayToView

type BindingConverter<'Control, 'Source> = {
    ToControl: 'Source -> 'Control
    ToSource: 'Control -> 'Source
}

type BindingInfo<'Control, 'ControlProp, 'SourceProp> = {
    Control: 'Control
    ControlProperty: PropertyInfo
    Source: obj
    SourceProperty: PropertyInfo
    BindingMode: BindingMode
    Converter: BindingConverter<'ControlProp, 'SourceProp> option
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

module CommonBinding =
    open BindingPatterns

    let createProxy createBinding (bindInfo: BindingInfo<_, _, _>) =
        let value = bindInfo.SourceProperty.GetValue bindInfo.Source
        let proxy = BindingProxy value
        let proxyBindInfo = { bindInfo with Source = proxy; SourceProperty = BindingProxy.Property }
        createBinding proxyBindInfo |> ignore
        { ModelProperty = bindInfo.SourceProperty
          ViewChanged = proxy.ViewChanged
          SetView = proxy.SetView
        }

    let controlPart<'Control, 'View> (controlProperty: Expr<'View>) =
        match controlProperty with
        | PropertyExpression (ctl, ctlProp) when typedefof<'Control>.IsAssignableFrom(ctl.GetType()) ->
            { Control = ctl :?> 'Control; ControlProperty = ctlProp } : BindViewPart<'Control, 'View>
        | _ -> failwith "Expected a property access expression on a control"

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
          Converter = None
        }
