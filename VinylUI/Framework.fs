namespace VinylUI

open System
open System.Reactive
open System.Reactive.Subjects
open System.Reactive.Linq
open System.Runtime.ExceptionServices
open System.Reflection
open System.Threading
open FSharp.Control

type EventHandler<'Model> =
    | Sync of ('Model -> 'Model)
    | Async of ('Model -> AsyncSeq<'Model>)

module Model =
    let equalIgnoreCase a b = String.Equals(a, b, StringComparison.InvariantCultureIgnoreCase)

    let computedProperties (props: PropertyInfo seq) =
        let ctorParams = lazy ((props |> Seq.head).DeclaringType.GetConstructors().[0].GetParameters()
                               |> Array.map (fun p -> p.Name))
        let isCtorParam (prop: PropertyInfo) =
            ctorParams.Value |> Seq.exists (equalIgnoreCase prop.Name)
        props |> Seq.filter (not << isCtorParam)

    let changes (props: PropertyInfo seq) (original: 'a) (updated: 'a) =
        props |> Seq.choose (fun p ->
            match p.GetValue updated, p.GetValue original with
            | upd, orig when upd <> orig -> Some (p, upd)
            | _ -> None
        )

    let permute (model: 'Model) changes =
        let t = typedefof<'Model>
        let ctor = t.GetConstructors().[0]
        let propNameIs name (prop: PropertyInfo) = equalIgnoreCase name prop.Name
        let props = t.GetProperties()
        let getCurrentValue propName = props |> Seq.find (propNameIs propName) |> (fun p -> p.GetValue model)
        let args =
            ctor.GetParameters()
            |> Array.map (fun param ->
                match changes |> Seq.tryFind (fst >> propNameIs param.Name) with
                | Some (_, newValue) -> box newValue
                | None -> getCurrentValue param.Name)
        ctor.Invoke(args) :?> 'Model

    let updateView bindings changes =
        changes |> Seq.iter (fun (prop, value) ->
            bindings
            |> Seq.filter (fun b -> b.ModelProperty = prop)
            |> Seq.iter (fun b -> b.SetView value))

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
        let bindings = binder view initialModel
        let props = typedefof<'Model>.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
        let computedProps = Model.computedProperties props

        let modelSubject = new BehaviorSubject<'Model>(initialModel) |> Signal

        // subscribe to control changes to update the model
        let exceptRef a = Seq.filter (fun x -> not <| obj.ReferenceEquals(x, a))
        bindings |> Seq.iter (fun binding ->
            binding.ViewChanged.Add (fun value ->
                let change = binding.ModelProperty, value
                let prevModel = modelSubject.Value
                modelSubject.Value <- Model.permute modelSubject.Value [change]
                let computedChanges = Model.changes computedProps prevModel modelSubject.Value |> Seq.toList
                Model.updateView (bindings |> exceptRef binding) (change :: computedChanges)))

        let eventList : IObservable<'Event> list = events view
        let eventStream = eventList.Merge()

        let updateModel originalModel newModel =
            let changes = newModel |> Model.changes props originalModel
            changes |> Model.updateView bindings
            modelSubject.Value <- Model.permute modelSubject.Value changes

        let subscription =
            Observer.Create(fun event ->
                match dispatcher event with
                | Sync handler ->
                    let handle () = 
                        handler modelSubject.Value
                        |> updateModel modelSubject.Value
                    match errorHandler with
                    | None -> handle ()
                    | Some errorHandler ->
                        try handle()
                        with e -> errorHandler e
                | Async handler ->
                    let gui = SynchronizationContext.Current
                    Async.StartWithContinuations(
                        computation = (async {
                            do! Async.SwitchToThreadPool()

                            let mutable originalModel = modelSubject.Value
                            do! handler originalModel |> AsyncSeq.iterAsync (fun newModel -> async {
                                do! Async.SwitchToContext(gui)
                                newModel |> updateModel originalModel
                                originalModel <- newModel
                                do! Async.SwitchToThreadPool()
                            })

                            do! Async.SwitchToContext(gui)
                        }),
                        continuation = id,
                        exceptionContinuation = (unwrapException >> (errorHandler |> Option.defaultValue defaultAsyncErrorHandler)),
                        cancellationContinuation = ignore)
            )
        #if DEBUG
            |> Observer.Checked
        #endif
            |> Observer.preventReentrancy
            |> Observer.notifyOnDispatcher
            |> eventStream.Subscribe

        (modelSubject :> ISignal<'Model>, subscription)
