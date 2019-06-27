module FrameworkTests

open System.ComponentModel
open NUnit.Framework
open FsUnitTyped
open VinylUI

type MyModel = {
    Name: string
    Score: int
}
with
    member this.Display = sprintf "%s: %i" this.Name this.Score

    static member NameProperty = typeof<MyModel>.GetProperty("Name")
    static member ScoreProperty = typeof<MyModel>.GetProperty("Score")
    static member DisplayProperty = typeof<MyModel>.GetProperty("Display")

[<Test>]
let ``Model.permute creates new model with new property value``() =
    let original = { Name = "before value"; Score = 1 }

    let newString = Model.permute original [| MyModel.NameProperty, box "after value" |]
    newString |> shouldEqual { original with Name = "after value" }

    let newInt = Model.permute original [| MyModel.ScoreProperty, box 7 |]
    newInt |> shouldEqual { original with Score = 7 }


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

    member val NameBox = Control<string>("")
    member val NameLabel = Control<string>("")
    member val ScoreInput = Control<int>(0)
    member val ScoreDisplay = Control<string>("")
    member val ScoreDisplay2 = Control<string>("")

type MyEvents =
    | Add of int
    | Reset

let initModel = { Name = "Bob"; Score = 1 }
let view = MyView()

let start multiBindCallback initModel =
    let binder (view: MyView) (model: MyModel) =
        [ Bind.viewInpc(<@ view.NameBox.Value @>).toModel(<@ model.Name @>)
          Bind.viewInpc(<@ view.ScoreInput.Value @>).toModelOneWay(<@ model.Score @>)
          Bind.model(<@ model.Display @>).toViewInpcOneWay(<@ view.ScoreDisplay.Value @>)
          Bind.model(<@ model.Name @>).toViewInpcOneWay(<@ view.NameLabel.Value @>)
          Bind.modelMulti(<@ model.Name, model.Score, model.Display @>).toFunc(fun (name, score, disp) ->
            multiBindCallback ()
            view.ScoreDisplay2.Value <- sprintf "%s scored %i" name score
          )
        ]

    let events (view: MyView) =
        [ view.Added |> Observable.map Add
          view.WasReset |> Observable.mapTo Reset
        ]

    let dispatcher = function
        | Add i -> Sync (fun m -> { m with Score = m.Score + i })
        | Reset -> Sync (fun _ -> initModel)

    Framework.start binder events dispatcher view initModel

[<Test>]
let ``Framework.start and full Sync exercise`` () =
    let mutable multiBindCalls = 0
    let callback () = multiBindCalls <- multiBindCalls + 1
    let model, sub = start callback initModel
    use __ = sub

    // creating the bindings should have updated the view...
    view.NameBox.Value |> shouldEqual "Bob"
    view.NameLabel.Value |> shouldEqual "Bob"
    view.ScoreDisplay.Value |> shouldEqual "Bob: 1"
    view.ScoreDisplay2.Value |> shouldEqual "Bob scored 1"
    // but one-way binding to model should not change view
    view.ScoreInput.Value |> shouldEqual 0

    // updating bound controls should update the model...
    view.NameBox.Value <- "Chad"
    model.Value |> shouldEqual { Name = "Chad"; Score = 1 }
    // and trigger bindings on model properties that changed
    view.ScoreDisplay.Value |> shouldEqual "Chad: 1"
    view.NameLabel.Value |> shouldEqual "Chad"
    view.ScoreDisplay2.Value |> shouldEqual "Chad scored 1"

    view.ScoreInput.Value <- 3
    model.Value |> shouldEqual { Name = "Chad"; Score = 3 }
    view.ScoreDisplay.Value |> shouldEqual "Chad: 3"
    view.ScoreDisplay2.Value |> shouldEqual "Chad scored 3"

    // triggering view events should raise the custom events and get handled by dispatcher
    view.Add 2
    model.Value |> shouldEqual { Name = "Chad"; Score = 5 }
    view.ScoreDisplay.Value |> shouldEqual "Chad: 5"
    view.ScoreDisplay2.Value |> shouldEqual "Chad scored 5"
    view.ScoreInput.Value |> shouldEqual 3
    multiBindCalls |> shouldEqual 4

    view.Reset ()
    model.Value |> shouldEqual { Name = "Bob"; Score = 1 }
    view.NameBox.Value |> shouldEqual "Bob"
    view.NameLabel.Value |> shouldEqual "Bob"
    view.ScoreDisplay2.Value |> shouldEqual "Bob scored 1"
    multiBindCalls |> shouldEqual 5

[<Test>]
let ``Framework.start does not fire to-view binding on same property from view changed`` () =
    let mutable bindingSetView = 0
    let toView x = bindingSetView <- bindingSetView + 1; x
    let binder (view: MyView) (model: MyModel) =
        [ Bind.viewInpc(<@ view.NameBox.Value @>).toModel(<@ model.Name @>, id, toView)
        ]

    let events _ = []
    let dispatcher _ = Sync id

    let model, sub = Framework.start binder events dispatcher view initModel
    use __ = sub

    bindingSetView |> shouldEqual 1
    view.NameBox.Value <- "Chad"
    bindingSetView |> shouldEqual 1

[<Test>]
let ``Model-to-view bindings fire in the order they are given`` () =
    let mutable triggered = []
    let binder (view: MyView) (model: MyModel) =
        [
          Bind.model(<@ model.Score @>).toFunc(fun _ -> triggered <- triggered @ ["Score"])
          Bind.model(<@ model.Name @>).toFunc(fun _ -> triggered <- triggered @ ["Name"])
        ]

    let events (view: MyView) =
        [ view.WasReset |> Observable.mapTo Reset ]

    let dispatcher = function
        | Reset -> Sync (fun _ -> initModel)
        | _ -> Sync id

    let model, sub = Framework.start binder events dispatcher view { Name = "Chad"; Score = 2 }
    use __ = sub
    triggered |> shouldEqual ["Score"; "Name"]

    triggered <- []
    view.Reset()
    triggered |> shouldEqual ["Score"; "Name"]
