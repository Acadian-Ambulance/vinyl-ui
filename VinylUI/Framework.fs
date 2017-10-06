namespace VinylUI

open System
open System.Reactive
open System.Reactive.Subjects
open System.Reactive.Linq
open System.Runtime.ExceptionServices
open System.Reflection

type EventHandler<'Model> =
    | Sync of ('Model -> 'Model)
    | Async of ('Model -> Async<'Model>)

module Model =
    let equalIgnoreCase a b =
        let r = String.Equals(a, b, StringComparison.InvariantCultureIgnoreCase)
        r

    let computedProperties (props: PropertyInfo seq) =
        let ctorParams = lazy ((props |> Seq.head).DeclaringType.GetConstructors().[0].GetParameters()
                               |> Array.map (fun p -> p.Name))
        let isCtorParam (prop: PropertyInfo) =
            ctorParams.Value |> Seq.exists (equalIgnoreCase prop.Name)
        props |> Seq.filter (not << isCtorParam)

    let changes (props: PropertyInfo seq) (original: 'a) (updated: 'a) =
        props |> Seq.choose (fun p ->
            let originalVal = p.GetValue original
            let updatedVal = p.GetValue updated
            if originalVal <> updatedVal then
                Some (p, updatedVal)
            else
                None
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

module Framework =
    let start binder events dispatcher (view: 'View) (initialModel: 'Model) =
        let error = fun(exn, _) -> ExceptionDispatchInfo.Capture(exn).Throw()

        let bindings = binder view initialModel
        let props = typedefof<'Model>.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)
        let computedProps = Model.computedProperties props

        let modelSubject = new BehaviorSubject<'Model>(initialModel)

        // subscribe to control changes to update the model
        let exceptRef a = Seq.filter (fun x -> not <| obj.ReferenceEquals(x, a))
        bindings |> Seq.iter (fun binding ->
            binding.ViewChanged.Add (fun value ->
                let change = binding.ModelProperty, value
                let prevModel = modelSubject.Value
                modelSubject.OnNext <| Model.permute modelSubject.Value [change]
                let computedChanges = Model.changes computedProps prevModel modelSubject.Value |> Seq.toList
                Model.updateView (bindings |> exceptRef binding) (change :: computedChanges)))

        let eventList : IObservable<'Event> list = events view
        let eventStream = eventList.Merge()

        let subscription =
            Observer.Create(fun event ->
                match dispatcher event with
                | Sync eventHandler ->
                    try
                        let changes = eventHandler modelSubject.Value |> Model.changes props modelSubject.Value
                        changes |> Model.updateView bindings
                        modelSubject.OnNext <| Model.permute modelSubject.Value changes
                    with exn -> error(exn, event)
                | Async eventHandler ->
                    Async.StartWithContinuations(
                        computation = async {
                            let originalModel = modelSubject.Value
                            let! newModel = eventHandler originalModel
                            return newModel |> Model.changes props originalModel
                        },
                        continuation = (fun changes ->
                            changes |> Model.updateView bindings
                            modelSubject.OnNext <| Model.permute modelSubject.Value changes
                        ),
                        exceptionContinuation = (fun exn -> error(exn, event)),
                        cancellationContinuation = ignore))
        #if DEBUG
            |> Observer.Checked
        #endif
            |> Observer.preventReentrancy
            |> Observer.notifyOnDispatcher
            |> eventStream.Subscribe

        (modelSubject, subscription)
