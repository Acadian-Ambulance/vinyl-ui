namespace VinylUI

open System
open System.ComponentModel
open System.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.Evaluator

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

module BindingConverters =
    let compose inner outer =
        { ToControl = inner.ToControl >> outer.ToControl
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
        { ToControl = makeGenericFunc toControl modelPropType
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

module BindingPatterns =
    let (|ControlExpression|_|) (expr: Expr option) =
        match expr with
        | Some controlExpr when typedefof<'Control>.IsAssignableFrom(controlExpr.Type) ->
            Some (QuotationEvaluator.EvaluateUntyped controlExpr :?> 'Control)
        | _ -> None

    let (|PropertyExpression|_|) = function
        | PropertyGet (Some source, propInfo, []) ->
            Some (QuotationEvaluator.EvaluateUntyped source, propInfo)
        | _ -> None

    let (|PropertyTupleSelector|_|) = function
        | Lambda (_, NewTuple [PropertyGet (_, displayProp, []); PropertyGet (_, valueProp, [])]) ->
            Some (displayProp, valueProp)
        | _ -> None

    let (|GenericType|_|) (t: Type) =
        if t.IsGenericType then
            Some (t.GetGenericTypeDefinition(), t.GetGenericArguments() |> List.ofArray)
        else None

    let (|GenericConstructor|_|) = function
        | NewObject (ctor, _) ->
            match ctor.DeclaringType with
            | GenericType (genType, [genArg]) -> Some (genType, genArg)
            | _ -> None
        | _ -> None

    let private (|ConvertModifier|_|) = function
        | SpecificCall <@@ Option.toNullable @@> (_, [genArg], _) ->
            Some (BindingConverters.optionToNullable genArg)
        | SpecificCall <@@ Option.toObj @@> (_, [genArg], _) ->
            Some (BindingConverters.optionToObj genArg)
        | GenericConstructor (genType, wrappedType) when genType = typedefof<Nullable<_>> ->
            Some (BindingConverters.wrapInNullable wrappedType)
        | _ -> None

    let private (|UpdateModeModifier|_|) = function
        | SpecificCall <@@ BindOption.UpdateSourceOnChange @@> _ -> Some OnChange
        | _ -> None

    let (|BindingModifier|_|) = function
        | ConvertModifier converter -> Some (Some converter, None)
        | UpdateModeModifier updateMode -> Some (None, Some updateMode)
        | _ -> None

    let rec (|InlineLambdaAndLet|) = function
        | Lambda (_, body) -> body
        | Let (letVar, letVal, InlineLambdaAndLet body) ->
            body.Substitute (fun var -> if var.Name = letVar.Name then Some letVal else None)
        | e -> e

    let (|PipedExpression|_|) = function
        | SpecificCall <@@ (|>) @@> (None, _, [left; InlineLambdaAndLet right]) -> Some (left, right)
        | _ -> None

    let rec (|BindingModifiers|) = function
        | PipedExpression (BindingModifiers (expr, leftConverter, leftUpdateMode),
                           BindingModifier (rightConverter, rightUpdateMode)) ->
            let converter = BindingConverters.composeOptions leftConverter rightConverter
            let updateMode =
                match leftUpdateMode, rightUpdateMode with
                | _, Some mode -> Some mode
                | _ -> leftUpdateMode
            (expr, converter, updateMode)
        | expr -> (expr, None, None)

    let rec (|BindPropertyExpression|_|) = function
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

    let (|BindExpression|_|) = function
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
                BindingMode = TwoWay updateMode
            }
        | _ -> None

    let private (|MaybePipedCall|_|) = function
        | PipedExpression (lastArg, Call (instance, methodInfo, callArgs)) ->
            // the right side is a lambda that calls the function with the lambda parameter as the last arg
            // swap the last arg with the left side of the pipe
            let args = callArgs |> List.rev |> List.tail |> List.append [lastArg] |> List.rev
            Some (instance, methodInfo, args)
        | Call (instance, methodInfo, args) -> Some (instance, methodInfo, args)
        | _ -> None

    let private toUntypedFunc (func: obj) =
        let rec findFuncType (t: Type) =
            if t.Name = "FSharpFunc`2" then t
            else if t.BaseType <> typedefof<obj> then findFuncType t.BaseType
            else failwith "Expected function value"
        let ftype = findFuncType (func.GetType())
        let argType = ftype.GetGenericArguments().[0]
        // create wrapping function that casts obj arg to inner func's arg type before calling it
        let var = Quotations.Var ("__v__", typedefof<obj>)
        let wrapper = Expr.Lambda (var, Expr.Application (Expr.Coerce (Expr.Value func, ftype),
                                                          Expr.Coerce (Expr.Var var, argType)))
        QuotationEvaluator.EvaluateUntyped wrapper :?> obj -> unit

    let private (|MaybePipedApplication|_|) = function
        | PipedExpression (arg, ValueWithName (func, _, _))
        | Application (ValueWithName (func, _, _), arg) ->
            Some (toUntypedFunc func, arg)
        | SpecificCall <@@ (|>) @@> (None, _, [arg; Lambda (var, body)])
        | Application (Lambda (var, body), arg) ->
            let newVar = Quotations.Var ((var.Name + "__"), typedefof<obj>)
            let body = body.Substitute (fun v ->
                if v.Name = var.Name then Some (Expr.Coerce (Expr.Var newVar, var.Type)) else None)
            let func = QuotationEvaluator.EvaluateUntyped (Expr.Lambda (newVar, body))
            Some (func :?> obj -> unit, arg)
        | _ -> None

    let rec private (|BindToViewFuncArgs|_|) = function
        | [PropertyExpression (src, prop)] -> Some ([], src, prop)
        | [Coerce (PropertyExpression (src, prop), _)] -> Some ([], src, prop)
        | argExpr :: BindToViewFuncArgs (args, src, prop) ->
            let arg =
                match argExpr with
                | QuoteTyped expr ->
                    box <| typedefof<Expr>.GetMethod("Cast").MakeGenericMethod(expr.Type).Invoke(null, [| expr |])
                | _ -> QuotationEvaluator.EvaluateUntyped argExpr
            Some (arg :: args, src, prop)
        | _ -> None

    let (|BindToViewFunc|_|) = function
        | MaybePipedCall (instanceExpr, methodInfo, BindToViewFuncArgs (otherArgs, src, prop)) ->
            let instance = instanceExpr |> Option.map QuotationEvaluator.EvaluateUntyped |> Option.toObj
            let updateView = (fun a ->
                let args = List.append otherArgs [a] |> List.toArray
                methodInfo.Invoke(instance, args) |> ignore)
            Some (src, prop, updateView)
        | MaybePipedApplication (func, PropertyExpression (src, prop)) -> Some (src, prop, func)
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

type BindingInfo<'c, 'cp, 'sp> with
    member this.CreateProxyBinding () =
        let value = this.SourceProperty.GetValue this.Source
        let proxy = BindingProxy value
        let proxyBindInfo = { this with Source = proxy; SourceProperty = BindingProxy.Property }
        let binding = {
            ModelProperty = this.SourceProperty
            ViewChanged = proxy.ViewChanged
            SetView = proxy.SetView
        }
        proxyBindInfo, binding

module CommonBinding =
    let private (|PropertyExpression|_|) = function
        | PropertyGet (Some source, propInfo, []) ->
            Some (QuotationEvaluator.EvaluateUntyped source, propInfo)
        | _ -> None

    let createProxy createBinding (bindInfo: BindingInfo<_, _, _>) =
        let proxyBindInfo, binding = bindInfo.CreateProxyBinding()
        createBinding proxyBindInfo |> ignore
        binding

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
        { Control = view.Control |> unbox<'Control>
          ControlProperty = view.ControlProperty
          Source = source.Source
          SourceProperty = source.SourceProperty
          BindingMode = mode
          Converter = None
        }


    /// Creates bindings from a quotation of statements, where each statement is an assignment to
    /// a view property from a model property or a call to a function passing a model property.
    let rec fromExpr createBinding assignExprs =
        let recurse = fromExpr createBinding
        match assignExprs with
        | Sequential (head, tail) -> List.append (recurse head) (recurse tail)
        | BindingPatterns.BindExpression bindInfo -> [createProxy createBinding bindInfo]
        | BindingPatterns.BindToViewFunc (source, property, updateView) ->
            updateView (property.GetValue source)
            let binding = {
                ModelProperty = property
                ViewChanged = Event<_>().Publish
                SetView = updateView
            }
            [binding]
        | e -> failwithf "Unrecognized binding expression: %A." e
