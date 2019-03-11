namespace VinylUI

open System
open System.Reactive
open System.Reactive.Subjects
open System.Reactive.Linq
open System.Runtime.ExceptionServices
open System.Reflection
open System.Threading
open FSharp.Control
open FSharp.Reflection

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
    let setErrorHandler (handler: Action<exn>) = errorHandler <- Some handler.Invoke

    let private defaultAsyncErrorHandler e = ExceptionDispatchInfo.Capture(e).Throw()

    let rec private unwrapException (e: Exception) =
        match e with
        | :? AggregateException as e -> e.InnerExceptions |> Seq.head |> unwrapException
        | _ -> e

    let start binder events dispatcher (view: 'View) (initialModel: 'Model) =
        let props = typedefof<'Model>.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
        let fields = FSharpType.GetRecordFields(typeof<'Model>)
        let computedProps = props |> Array.except fields
        let bindings = binder view initialModel |> Seq.toArray
        let bindingsFor prop = bindings |> Array.filter (fun b -> b.ModelProperties |> List.contains prop)
        let propBindings = props |> Array.map (fun p -> (p, bindingsFor p)) |> dict
        let bindingsTriggered changes = changes |> Seq.collect (fst >> propBindings.get_Item) |> Seq.distinct

        let modelSubject = new BehaviorSubject<'Model>(initialModel) |> Signal

        // subscribe to control changes to update the model
        bindings |> Seq.iter (fun binding ->
            binding.ViewChanged |> Option.iter (fun vc -> vc.Add (fun value ->
                let prop = binding.ModelProperties.Head // multi-binding from view not supported
                let change = (prop, value)
                let prevModel = modelSubject.Value
                modelSubject.Value <- Model.permute modelSubject.Value [| change |]
                // update view for other bindings that changed
                Model.diff (Array.append [| prop |] computedProps) prevModel modelSubject.Value
                |> bindingsTriggered
                |> Seq.filter (fun b -> b.ModelProperties <> [prop] || b.ViewChanged.IsNone)
                |> Model.updateView modelSubject.Value
            ))
        )

        let updateModel prevModel newModel =
            let changes = Model.diff props prevModel newModel |> Seq.toArray
            modelSubject.Value <- Model.permute modelSubject.Value changes
            Model.updateView modelSubject.Value (bindingsTriggered changes)

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
                computation = (async {
                    let mutable prevModel = modelSubject.Value
                    do! handler prevModel |> AsyncSeq.iterAsync (fun newModel -> async {
                        newModel |> updateModel prevModel
                        prevModel <- newModel
                    })
                }),
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

[<RequireQualifiedAccess>]
module Observable =
    let mapTo value = Observable.map (fun _ -> value)
