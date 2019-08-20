[<AutoOpen>]
module Prelude

open System.ComponentModel

type InpcControl<'a when 'a: equality>(initVal: 'a) =
    let mutable value = initVal
    let propChanged = Event<_,_>()

    member this.Value
        with get () = value
        and set v =
            if value <> v then
                value <- v
                propChanged.Trigger(this, PropertyChangedEventArgs("Value"))

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propChanged.Publish
