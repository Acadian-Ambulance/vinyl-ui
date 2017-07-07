namespace VinylUI

open System
open System.ComponentModel
open System.Reflection
open System.Reactive
open System.Reactive.Linq
open System.Runtime.ExceptionServices

type EventHandler<'Model> =
    | Sync of ('Model -> 'Model)
    | Async of ('Model -> Async<'Model>)

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

module Framework =
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

    let permuteModel (model: 'Model) propertyName value =
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

    let start (binder: 'View -> 'Model -> Binding list) (events: 'View -> IObservable<'Event> list) dispatcher (view: 'View) (model: 'Model) =
        let error = fun(exn, _) -> ExceptionDispatchInfo.Capture(exn).Throw()

        let bindings = binder view model

        let mutable currentModel = model

        // subscribe to control changes to update the model
        bindings |> Seq.iter (fun binding ->
            binding.ViewChanged.Add (fun value ->
                currentModel <- permuteModel currentModel binding.ModelProperty.Name value))

        let eventStream = (events view).Merge()

        Observer.Create(fun event ->
            match dispatcher event with
            | Sync eventHandler ->
                try
                    let newModel = eventHandler currentModel
                    // update bindings for model changes
                    newModel
                        |> changes currentModel
                        |> Seq.iter (fun (prop, value) ->
                            bindings
                            |> Seq.filter (fun b -> b.ModelProperty = prop)
                            |> Seq.iter (fun b -> b.SetView value))
                    // for async, this will need to merge the changes with currentModel
                    currentModel <- newModel
                with exn -> error(exn, event)
            | Async eventHandler ->
                Async.StartWithContinuations(
                    computation = eventHandler currentModel,
                    continuation = ignore, // todo
                    exceptionContinuation = (fun exn -> error(exn, event)),
                    cancellationContinuation = ignore))
    #if DEBUG
        |> Observer.Checked
    #endif
        |> Observer.preventReentrancy
        |> Observer.notifyOnDispatcher
        |> eventStream.Subscribe
