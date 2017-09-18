namespace VinylUI

open System
open System.Reactive
open System.Reactive.Linq
open System.Runtime.ExceptionServices
open System.Reflection

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

    let permute (model: 'Model) changes =
        let t = typedefof<'Model>
        let ctor = t.GetConstructors().[0]
        let propNameIs name (prop: PropertyInfo) = String.Equals(name, prop.Name, StringComparison.InvariantCultureIgnoreCase)
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

        let mutable currentModel = initialModel

        // subscribe to control changes to update the model
        let exceptRef a = Seq.filter (fun x -> not <| obj.ReferenceEquals(x, a))
        bindings |> Seq.iter (fun binding ->
            binding.ViewChanged.Add (fun value ->
                let prop = binding.ModelProperty
                currentModel <- Model.permute currentModel [prop, value]
                Model.updateView (bindings |> exceptRef binding) [prop, value]))

        let eventList : IObservable<'Event> list = events view
        let eventStream = eventList.Merge()

        Observer.Create(fun event ->
            match dispatcher event with
            | Sync eventHandler ->
                try
                    let changes = eventHandler currentModel |> Model.changes currentModel
                    changes |> Model.updateView bindings
                    currentModel <- Model.permute currentModel changes
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
                        currentModel <- Model.permute currentModel changes
                    ),
                    exceptionContinuation = (fun exn -> error(exn, event)),
                    cancellationContinuation = ignore))
    #if DEBUG
        |> Observer.Checked
    #endif
        |> Observer.preventReentrancy
        |> Observer.notifyOnDispatcher
        |> eventStream.Subscribe
