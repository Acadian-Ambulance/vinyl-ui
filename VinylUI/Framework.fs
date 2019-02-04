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
    let private equalIgnoreCase a b = String.Equals(a, b, StringComparison.InvariantCultureIgnoreCase)

    let isComputedProperty (typ: Type) =
        let ctorParams = typ.GetConstructors().[0].GetParameters() |> Array.map (fun p -> p.Name)
        let isCtorParam (prop: PropertyInfo) =
            ctorParams |> Seq.exists (equalIgnoreCase prop.Name)
        not << isCtorParam

    let permute (model: 'Model) changes =
        let t = typeof<'Model>
        let ctor = t.GetConstructors().[0]
        let propNameIs name (prop: PropertyInfo) = equalIgnoreCase name prop.Name
        let props = t.GetProperties()
        let getCurrentValue propName = props |> Seq.find (propNameIs propName) |> (fun p -> p.GetValue model)
        let args =
            ctor.GetParameters()
            |> Array.map (fun param ->
                match changes |> Seq.tryFind (fst >> propNameIs param.Name) with
                | Some (_, newValue) -> box newValue
                | None -> getCurrentValue param.Name
            )
        ctor.Invoke(args) :?> 'Model

    let change (prevModel: 'a) (newModel: 'a) (prop: PropertyInfo) =
        match prop.GetValue prevModel, prop.GetValue newModel with
        | prev, new' when new' <> prev -> Some (prop, new')
        | _ -> None

    let diff props prevModel newModel =
        props |> Seq.choose (change prevModel newModel)

    let updateView changes =
        changes |> Seq.iter (fun (bindings, newValue) ->
            bindings |> Seq.iter (fun b -> b.SetView newValue)
        )

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
        let computedProps = props |> Array.filter (Model.isComputedProperty typeof<'Model>)
        let bindings = binder view initialModel |> Seq.toArray
        let propBindings = props |> Array.map (fun p -> (p, bindings |> Array.filter (fun b -> b.ModelProperty = p)))
                                 |> dict
        let bindingChanges = Seq.map (fun (prop, value) -> (propBindings.[prop], value))

        let modelSubject = new BehaviorSubject<'Model>(initialModel) |> Signal

        // subscribe to control changes to update the model
        bindings |> Seq.iter (fun binding ->
            binding.ViewChanged.Add (fun value ->
                let change = (binding.ModelProperty, value)
                let prevModel = modelSubject.Value
                modelSubject.Value <- Model.permute modelSubject.Value [change]
                // update view for other bindings on the same property
                [ (propBindings.[binding.ModelProperty] |> Array.except [binding], value) ]
                |> Model.updateView
                // update view for bindings on computed properties that changed
                Model.diff computedProps prevModel modelSubject.Value
                |> bindingChanges
                |> Model.updateView
            )
        )

        let updateModel prevModel newModel =
            let changes = Model.diff props prevModel newModel
            modelSubject.Value <- Model.permute modelSubject.Value changes
            Model.updateView (bindingChanges changes)

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
            let gui = SynchronizationContext.Current
            Async.StartWithContinuations(
                computation = (async {
                    do! Async.SwitchToThreadPool()

                    let mutable prevModel = modelSubject.Value
                    do! handler prevModel |> AsyncSeq.iterAsync (fun newModel -> async {
                        do! Async.SwitchToContext(gui)
                        newModel |> updateModel prevModel
                        prevModel <- newModel
                        do! Async.SwitchToThreadPool()
                    })

                    do! Async.SwitchToContext(gui)
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
