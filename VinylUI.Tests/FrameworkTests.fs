module FrameworkTests

open System.Reflection
open NUnit.Framework
open FsUnitTyped
open VinylUI

type Record = {
    Text: string
    Number: int
}
with
    static member TextProperty = typedefof<Record>.GetProperty("Text")
    static member NumberProperty = typedefof<Record>.GetProperty("Number")
    static member InflatedNumberProperty = typedefof<Record>.GetProperty("InflatedNumber")

    member this.InflatedNumber = this.Number + 10

let props = typedefof<Record>.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)

[<Test>]
let ``Model.computedProperties returns only computed properties``() =
    Model.computedProperties props |> Seq.toList |> shouldEqual [ Record.InflatedNumberProperty ]

[<Test>]
let ``Model.changes detects single change``() =
    let original = { Text = "before value"; Number = 7 }
    let updated = { Text = "after value"; Number = 7 }
    let changes = Model.changes props original updated |> Seq.toList
    changes |> shouldEqual [ Record.TextProperty, box "after value" ]

[<Test>]
let ``Model.changes detects multiple changes``() =
    let original = { Text = "before value"; Number = 1 }
    let updated = { Text = "after value"; Number = 7 }
    let changes = Model.changes props original updated |> Seq.toList
    changes |> shouldEqual [ Record.TextProperty, box "after value";
                             Record.NumberProperty, box 7
                             Record.InflatedNumberProperty, box 17 ]

[<Test>]
let ``Model.permute creates new model with new property value``() =
    let original = { Text = "before value"; Number = 1 }

    let newString = Model.permute original [Record.TextProperty, "after value"]
    newString |> shouldEqual { original with Text = "after value" }

    let newInt = Model.permute original [Record.NumberProperty, 7]
    newInt |> shouldEqual { original with Number = 7 }

[<Test>]
let ``Model.updateView updates all bindings for a property``() =
    let mutable boundString1 = null
    let mutable boundString2 = null
    let mutable boundNumber = null
    let bindings = [
        { ModelProperty = Record.TextProperty; SetView = (fun v -> boundString1 <- v); ViewChanged = null }
        { ModelProperty = Record.NumberProperty; SetView = (fun v -> boundNumber <- v); ViewChanged = null }
        { ModelProperty = Record.TextProperty; SetView = (fun v -> boundString2 <- v); ViewChanged = null }
    ]

    let newValue = box "test"
    Model.updateView bindings [Record.TextProperty, newValue]

    boundString1 |> shouldEqual newValue
    boundString2 |> shouldEqual newValue
    boundNumber |> shouldEqual null


open System.ComponentModel

type Control<'a when 'a : equality>(initValue: 'a) =
    let mutable value = initValue
    let changed = Event<_,_>()

    member this.Value
        with get () = value
        and set v =
            if value <> v then
                value <- v
                changed.Trigger(null, PropertyChangedEventArgs("Value"))

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = changed.Publish

type MyView() =
    let add = Event<int>()
    let reset = Event<unit>()
    member val Added = add.Publish
    member this.Add i = add.Trigger i
    member val WasReset = reset.Publish
    member this.Reset () = reset.Trigger ()

    member val TextBox = Control<string>("")
    member val Label = Control<string>("")
    member val NumberBox = Control<int>(0)

type MyModel = {
    Name: string
    Score: int
}
with
    member this.Description = sprintf "%s: %i" this.Name this.Score

type MyEvents =
    | Add of int
    | Reset

[<Test>]
let ``Framework.start and full Sync exercise`` () =
    let binder (view: MyView) (model: MyModel) =
        [ Bind.viewInpc(<@ view.TextBox.Value @>).toModel(<@ model.Name @>)
          Bind.viewInpc(<@ view.NumberBox.Value @>).toModelOneWay(<@ model.Score @>)
          Bind.model(<@ model.Description @>).toViewInpcOneWay(<@ view.Label.Value @>)
        ]

    let events (view: MyView) =
        [ view.Added |> Observable.map Add
          view.WasReset |> Observable.mapTo Reset
        ]

    let initModel = { Name = "Bob"; Score = 1 }

    let dispatcher = function
        | Add i -> Sync (fun m -> { m with Score = m.Score + i })
        | Reset -> Sync (fun _ -> initModel)

    let view = MyView()
    let model, sub = Framework.start binder events dispatcher view initModel
    use __ = sub

    // creating the bindings should have updated the view...
    view.TextBox.Value |> shouldEqual "Bob"
    view.Label.Value |> shouldEqual "Bob: 1"
    // but one-way binding to model should not change view
    view.NumberBox.Value |> shouldEqual 0

    // updating bound controls should update the model...
    view.TextBox.Value <- "Chad"
    model.Value |> shouldEqual { Name = "Chad"; Score = 1 }
    // and trigger bindings on read-only properties that changed
    view.Label.Value |> shouldEqual "Chad: 1"

    view.NumberBox.Value <- 3
    model.Value |> shouldEqual { Name = "Chad"; Score = 3 }
    view.Label.Value |> shouldEqual "Chad: 3"

    // triggering view events should raise the custom events and get handled by dispatcher
    view.Add 2
    model.Value |> shouldEqual { Name = "Chad"; Score = 5 }
    view.Label.Value |> shouldEqual "Chad: 5"
    view.NumberBox.Value |> shouldEqual 3

    view.Reset ()
    model.Value |> shouldEqual { Name = "Bob"; Score = 1 }
    view.TextBox.Value |> shouldEqual "Bob"
