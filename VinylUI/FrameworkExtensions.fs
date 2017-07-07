namespace VinylUI

open System

[<RequireQualifiedAccess>]
module internal Observer =
    open System.Reactive
    open System.Windows.Threading

    let notifyOnDispatcher(observer: IObserver<_>) =
        let dispatcher = Dispatcher.CurrentDispatcher
        let invokeOnDispatcher f = if dispatcher.CheckAccess() then f() else dispatcher.InvokeAsync f |> ignore
        {
            new IObserver<_> with
                member __.OnNext value = invokeOnDispatcher(fun() -> observer.OnNext value)
                member __.OnError error = invokeOnDispatcher(fun() -> observer.OnError error)
                member __.OnCompleted() = invokeOnDispatcher observer.OnCompleted
        }

    let preventReentrancy observer = Observer.Synchronize(observer, preventReentrancy = true)

[<RequireQualifiedAccess>]
module Observable =
    let mapTo value = Observable.map(fun _ -> value)
