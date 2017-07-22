namespace VinylUI

open System
open System.ComponentModel
open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.Evaluator

type BindingUpdateMode =
    | OnValidation
    | OnChange
    | Never

type BindingConverter<'Source, 'Control> = {
    ToControl: 'Source -> 'Control
    ToSource: 'Control -> 'Source
}

type BindingInfo<'Control> = {
    Control: 'Control
    ControlProperty: PropertyInfo
    Source: obj
    SourceProperty: PropertyInfo
    Converter: BindingConverter<obj, obj> option
    UpdateMode: BindingUpdateMode option
}

module BindingConverters =
    let compose inner outer = {
        ToControl = inner.ToControl >> outer.ToControl
        ToSource = outer.ToSource >> inner.ToSource
    }

    let composeOptions inner outer =
        match inner, outer with
        | Some lc, Some rc -> Some <| compose lc rc
        | Some lc, None -> Some lc
        | None, rc -> rc

    let private makeGenericConverter (toControl: Expr<'s -> 'c>) (toSource: Expr<'c -> 's>) modelPropType =
        let makeGenericFunc funcExpr genericArg =
            match funcExpr with
            | Lambda (_, Call (None, mi, _)) ->
                let funcOfT = mi.GetGenericMethodDefinition().MakeGenericMethod([|genericArg|])
                (fun x -> funcOfT.Invoke(null, [|x|]))
            | _ -> failwith "expression is not a static function"
        {
            ToControl = makeGenericFunc toControl modelPropType
            ToSource = makeGenericFunc toSource modelPropType
        }

    let optionToNullable t = makeGenericConverter <@ Option.toNullable @> <@ Option.ofNullable @> t

    let private _optionToObj opt =
        match opt with
        | Some v -> box v
        | None -> null
    let private _optionFromObj (o: obj) : 'a option =
        match o with
        | null -> None
        | v -> Some (v :?> 'a)
    let optionToObj = makeGenericConverter <@ _optionToObj @> <@ _optionFromObj @>

    let private _wrapInNullable = Nullable
    let private _unwrapNullable (v: Nullable<'a>) = v.GetValueOrDefault()
    let wrapInNullable t = makeGenericConverter <@ _wrapInNullable @> <@ _unwrapNullable @> t

module BindOption =
    let private undefined (x: 'a) : 'a = failwith "for quotation use only"

    /// When used in a binding quotation, specifies that the resulting binding should update the bound source when
    /// the control property changes. The default is to update on validation.
    let UpdateSourceOnChange = undefined
    /// When used in a binding quotation, specifies that the resulting binding should not update the bound source
    /// (one way binding). The default is to update on validation.
    let UpdateSourceNever = undefined

module BindingPatterns =
    let (|ControlExpression|_|) (expr: Expr option) =
        match expr with
        | Some controlExpr when typedefof<'Control>.IsAssignableFrom(controlExpr.Type) ->
            Some (QuotationEvaluator.EvaluateUntyped controlExpr :?> 'Control)
        | _ -> None

    let (|PropertyExpression|_|) (expr: Expr) =
        match expr with
        | PropertyGet (Some source, propInfo, []) ->
            Some (QuotationEvaluator.EvaluateUntyped source, propInfo)
        | _ -> None

    let (|PropertyTupleSelector|_|) expr =
        match expr with
        | Lambda (_, NewTuple [PropertyGet (_, displayProp, []); PropertyGet (_, valueProp, [])]) ->
            Some (displayProp, valueProp)
        | _ -> None

    let (|GenericType|_|) (t: Type) =
        if t.IsGenericType then
            Some (t.GetGenericTypeDefinition(), t.GetGenericArguments() |> List.ofArray)
        else None

    let (|GenericConstructor|_|) (expr: Expr) =
        match expr with
        | NewObject (ctor, _) ->
            match ctor.DeclaringType with
            | GenericType (genType, [genArg]) -> Some (genType, genArg)
            | _ -> None
        | _ -> None

    let private (|ConvertModifier|_|) (expr: Expr) =
        match expr with
        | SpecificCall <@@ Option.toNullable @@> (_, [genArg], _) ->
            Some (BindingConverters.optionToNullable genArg)
        | GenericConstructor (genType, wrappedType) when genType = typedefof<Nullable<_>> ->
            Some (BindingConverters.wrapInNullable wrappedType)
        | _ -> None

    let private (|UpdateModeModifier|_|) (expr: Expr) =
        match expr with
        | SpecificCall <@@ BindOption.UpdateSourceOnChange @@> _ -> Some OnChange
        | SpecificCall <@@ BindOption.UpdateSourceNever @@> _ -> Some Never
        | _ -> None

    let (|BindingModifier|_|) (expr: Expr) =
        match expr with
        | ConvertModifier converter -> Some (Some converter, None)
        | UpdateModeModifier updateMode -> Some (None, Some updateMode)
        | _ -> None

    let (|PipedExpression|_|) (expr: Expr) =
        match expr with
        | SpecificCall <@@ (|>) @@> (None, _, [left; Lambda (_, right)]) -> Some (left, right)
        | SpecificCall <@@ (|>) @@> (None, _, [left; right]) -> Some (left, right)
        | _ -> None

    let rec (|BindingModifiers|) (expr: Expr) =
        match expr with
        | PipedExpression (BindingModifiers (expr, leftConverter, leftUpdateMode),
                           BindingModifier (rightConverter, rightUpdateMode)) ->
            let converter = BindingConverters.composeOptions leftConverter rightConverter
            let updateMode =
                match leftUpdateMode, rightUpdateMode with
                | _, Some mode -> Some mode
                | _ -> leftUpdateMode
            (expr, converter, updateMode)
        | _ -> (expr, None, None)

    let rec (|BindPropertyExpression|_|) (expr: Expr) =
        match expr with
        | BindingModifiers (PropertyExpression (src, srcMember), converter, updateMode) ->
            Some (src, srcMember, converter, updateMode)
        | Coerce (BindPropertyExpression (src, srcMember, converter, updateMode), _) ->
            let newConverter =
                match converter, srcMember.PropertyType with
                | Some c, _ -> Some c
                | None, GenericType (genType, [genArg]) when genType = typedefof<option<_>> ->
                    Some <| BindingConverters.optionToObj genArg
                | _ -> None
            Some (src, srcMember, newConverter, updateMode)
        | _ -> None

    let (|BindExpression|_|) (assignExpr: Expr) =
        match assignExpr with
        | PropertySet (ControlExpression control,
                       controlProp,
                       [],
                       BindPropertyExpression (source, sourceProp, converter, updateMode)) ->
            Some {
                Control = control
                ControlProperty = controlProp
                Source = source
                SourceProperty = sourceProp
                Converter = converter
                UpdateMode = updateMode
            }
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
        and set v = viewChanged.Trigger v

    member this.SetView v =
        value <- v
        modelChanged.Trigger(null, PropertyChangedEventArgs("Value"))

    static member Property = typedefof<BindingProxy>.GetProperty("Value")

type Binding = {
    ModelProperty: PropertyInfo
    ViewChanged: IObservable<obj>
    SetView: obj -> unit
}

type BindingInfo<'Control> with
    member this.CreateProxyBinding () =
        let value = this.SourceProperty.GetValue this.Source
        let proxy = BindingProxy value
        let proxyBindInfo = { this with Source = proxy; SourceProperty = BindingProxy.Property }
        let binding = {
            ModelProperty = this.SourceProperty
            ViewChanged = proxy.ViewChanged
            SetView = (fun v -> proxy.Value <- v)
        }
        proxyBindInfo, binding
