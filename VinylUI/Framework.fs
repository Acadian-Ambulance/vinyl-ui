namespace VinylUI

open System
open System.Reactive
open System.Reactive.Subjects
open System.Reactive.Linq
open System.Runtime.ExceptionServices
open FSharp.Control

type EventHandler<'Model> =
    | Sync of ('Model -> 'Model)
    | Async of ('Model -> AsyncSeq<'Model>)

type ISignal<'a> =
    inherit IObservable<'a>
    abstract Value: 'a with get

module Framework =
    type Signal<'a>(subject: BehaviorSubject<'a>) =
        interface ISignal<'a> with
            member this.Subscribe o = subject.Subscribe o
            member this.Value = subject.Value

        member this.Value
            with get () = subject.Value
            and set value = subject.OnNext value

    let mutable private errorHandler : (exn -> unit) option = None

    /// Set an error handler for uncaught exceptions thrown by event handlers
    let setErrorHandler handler = errorHandler <- Some handler

    /// Set an error handler as a System.Action for uncaught exceptions thrown by event handlers
    let setErrorHandlerAction (handler: Action<exn>) = errorHandler <- Some handler.Invoke

    let private defaultAsyncErrorHandler e = ExceptionDispatchInfo.Capture(e).Throw()

    let rec private unwrapException (e: Exception) =
        match e with
        | :? AggregateException as e -> e.InnerExceptions |> Seq.head |> unwrapException
        | _ -> e

    let start binder events dispatcher (view: 'View) (initialModel: 'Model) =
        let props = Model.getProps typeof<'Model>
        let computedProps = Model.getComputedProps typeof<'Model>
        let bindings = binder view initialModel |> Seq.toArray
        let bindingsTriggered (changes: (PropertyChain * _) seq) =
            let changed = changes |> Seq.map fst |> Seq.toList
            bindings |> Array.filter (fun b ->
                b.ModelProperties
                |> Seq.allPairs changed
                |> Seq.exists (fun (change, bound) -> Seq.forall2 (=) bound.Chain change.Chain)
            )

        let modelSubject = new BehaviorSubject<'Model>(initialModel) |> Signal
        let mutable pendingBindings = []

        // subscribe to control changes to update the model
        bindings |> Array.iter (fun binding ->
            binding.ViewChanged |> Option.iter (fun vc -> vc.Add (fun value ->
                let prop = binding.ModelProperties.Head // multi-binding from view not supported
                if not (pendingBindings |> List.contains prop) then
                    let change = (prop, value)
                    let prevModel = modelSubject.Value
                    modelSubject.Value <- Model.permute modelSubject.Value [| change |]
                    // update view for other bindings that changed
                    Model.diff (prop.Chain.Head :: computedProps) prevModel modelSubject.Value
                    |> bindingsTriggered
                    |> Seq.filter (fun b -> b.ModelProperties <> [prop] || b.ViewChanged.IsNone)
                    |> Seq.iter (Model.updateView modelSubject.Value)
            ))
        )

        let updateModel prevModel newModel =
            let changes = Model.diff props prevModel newModel |> Seq.toArray
            modelSubject.Value <- Model.permute modelSubject.Value changes
            let rec update bindings =
                pendingBindings <- bindings |> List.collect (fun b -> b.ModelProperties)
                match bindings with
                | binding :: rest ->
                    Model.updateView modelSubject.Value binding
                    update rest
                | [] -> ()
            update (bindingsTriggered changes |> Seq.toList)

        let runSync handler =
            let handle () = 
                handler modelSubject.Value
                |> updateModel modelSubject.Value
            match errorHandler with
            | None -> handle ()
            | Some errorHandler ->
                try handle()
                with e -> errorHandler e

        let runAsync handler =
            Async.StartWithContinuations(
                computation = async {
                    let mutable prevModel = modelSubject.Value
                    do! handler prevModel |> AsyncSeq.iterAsync (fun newModel -> async {
                        newModel |> updateModel prevModel
                        prevModel <- newModel
                    })
                },
                continuation = id,
                exceptionContinuation =
                    (unwrapException >> (errorHandler |> Option.defaultValue defaultAsyncErrorHandler)),
                cancellationContinuation = ignore
            )

        let eventList : IObservable<'Event> list = events view
        let eventStream = eventList.Merge()

        let subscription =
            Observer.Create(fun event ->
                match dispatcher event with
                | Sync handler -> runSync handler
                | Async handler -> runAsync handler
            )
        #if DEBUG
            |> Observer.Checked
        #endif
            |> fun o -> Observer.Synchronize(o, preventReentrancy = true)
            |> eventStream.Subscribe

        (modelSubject :> ISignal<'Model>, subscription)

/// Computation builder that allows creating of asynchronous sequences using the 'asyncSeq { ... }' syntax
/// This is a "fixed" version without the value-ignoring Return present in FSharp.Control.AsyncSeq 2.x
type FixedAsyncSeqBuilder() =
    let builder = AsyncSeq.AsyncSeqBuilder()

    member _.Bind (source, body) = builder.Bind (source, body)
    member _.Combine (seq1, seq2) = builder.Combine (seq1, seq2)
    member _.Delay f = builder.Delay f
    member _.For (source: 'a seq, action) = builder.For (source, action)
    member _.For (source: 'a AsyncSeq, action) = builder.For (source, action)
    member _.TryFinally (body, comp) = builder.TryFinally (body, comp)
    member _.TryWith (body, handler) = builder.TryWith (body, handler)
    member _.Using (resource, binder) = builder.Using (resource, binder)
    member _.While (guard, body) = builder.While (guard, body)
    member _.Yield value = builder.Yield value
    member _.YieldFrom source = builder.YieldFrom source
    member _.Zero () = builder.Zero ()

[<AutoOpen>]
module Prelude =
    let asyncSeq = FixedAsyncSeqBuilder()

[<RequireQualifiedAccess>]
module Observable =
    let mapTo value = Observable.map (fun _ -> value)
