namespace VinylUI

open System
open System.Reactive
open System.Reactive.Linq
open System.Runtime.ExceptionServices

type EventHandler<'Model> =
    | Sync of ('Model -> 'Model)
    | Async of ('Model -> Async<'Model>)

module Model =
    let changes (original: 'a) (updated: 'a) =
        typedefof<'a>.GetProperties()
        |> Seq.choose (fun p ->
            let originalVal = p.GetValue original
            let updatedVal = p.GetValue updated
            if originalVal <> updatedVal then
                Some (p, updatedVal)
            else
                None
        )

    // TODO: accept multiple changes
    let permute (model: 'Model) propertyName value =
        let t = typedefof<'Model>
        let ctor = t.GetConstructors().[0]
        let props = t.GetProperties()
        let equalIgnoreCase s1 s2 = String.Equals(s1, s2, StringComparison.InvariantCultureIgnoreCase)
        let args =
            ctor.GetParameters()
            |> Array.map (fun param ->
                if equalIgnoreCase param.Name propertyName then
                    value :> obj
                else
                    let prop = props |> Seq.find (fun p -> equalIgnoreCase p.Name param.Name)
                    prop.GetValue model)
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

        let mutable currentModel = initialModel

        // subscribe to control changes to update the model
        bindings |> Seq.iter (fun binding ->
            binding.ViewChanged.Add (fun value ->
                let prop = binding.ModelProperty
                currentModel <- Model.permute currentModel prop.Name value
                let otherBindings = bindings |> Seq.filter (fun b -> not <| obj.ReferenceEquals(b, binding))
                Model.updateView otherBindings [prop, value]))

        let eventList : IObservable<'Event> list = events view
        let eventStream = eventList.Merge()

        Observer.Create(fun event ->
            match dispatcher event with
            | Sync eventHandler ->
                try
                    let newModel = eventHandler currentModel
                    newModel |> Model.changes currentModel |> Model.updateView bindings
                    currentModel <- newModel
                with exn -> error(exn, event)
            | Async eventHandler ->
                Async.StartWithContinuations(
                    computation = async {
                        let originalModel = currentModel
                        let! newModel = eventHandler originalModel
                        return newModel |> Model.changes originalModel
                    },
                    continuation = (fun changes ->
                        changes |> Model.updateView bindings
                        changes |> Seq.iter (fun (prop, value) ->
                            currentModel <- Model.permute currentModel prop.Name value)
                    ),
                    exceptionContinuation = (fun exn -> error(exn, event)),
                    cancellationContinuation = ignore))
    #if DEBUG
        |> Observer.Checked
    #endif
        |> Observer.preventReentrancy
        |> Observer.notifyOnDispatcher
        |> eventStream.Subscribe
