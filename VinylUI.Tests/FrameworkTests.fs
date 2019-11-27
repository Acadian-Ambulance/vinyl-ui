module FrameworkTests

open NUnit.Framework
open FsUnitTyped
open VinylUI

type ScoreModel = {
    Name: string
    Score: int
}
with
    member this.Display = sprintf "%s: %i" this.Name this.Score

    static member Default = { Name = "Bob"; Score = 1 }

    static member NameProperty = typeof<ScoreModel>.GetProperty("Name")
    static member ScoreProperty = typeof<ScoreModel>.GetProperty("Score")
    static member DisplayProperty = typeof<ScoreModel>.GetProperty("Display")

type ScoreboardModel = {
    Home: ScoreModel
    Away: ScoreModel
}
with
    static member Default = {
        Home = { Name = "Stanky Pankies"; Score = 1 }
        Away = { Name = "Lackin Cracklins"; Score = 1 }
    }

    static member HomeProperty = typeof<ScoreboardModel>.GetProperty("Home")
    static member AwayProperty = typeof<ScoreboardModel>.GetProperty("Away")


[<Test>]
let ``Model.permute creates new model with new property value``() =
    let original = { Name = "before"; Score = 1 }

    let nameProp = PropertyChain [ScoreModel.NameProperty]
    let newString = Model.permute original [| nameProp, box "after" |]
    newString |> shouldEqual { original with Name = "after" }

    let scoreProp = PropertyChain [ScoreModel.ScoreProperty]
    let newInt = Model.permute original [| scoreProp, box 7 |]
    newInt |> shouldEqual { original with Score = 7 }

[<Test>]
let ``Model.permute nested property creates new model with new property value``() =
    let original = ScoreboardModel.Default

    let homeScoreProp = PropertyChain [ScoreboardModel.HomeProperty; ScoreModel.ScoreProperty]
    let after = Model.permute original [| homeScoreProp, box 9 |]
    after |> shouldEqual { original with Home = { original.Home with Score = 9 } }

[<Test>]
let ``Model.diff nested model returns change for only nested property``() =
    let before = ScoreboardModel.Default
    let after = { before with Home = { before.Home with Score = 2 } }
    let changes = Model.diff [ScoreboardModel.HomeProperty; ScoreboardModel.AwayProperty] before after

    let expected = [
        (PropertyChain [ScoreboardModel.HomeProperty; ScoreModel.ScoreProperty], box after.Home.Score)
        (PropertyChain [ScoreboardModel.HomeProperty; ScoreModel.DisplayProperty], box after.Home.Display)
    ]
    changes |> Seq.toList |> shouldEqual expected


type MyView() =
    let add = Event<int>()
    let reset = Event<unit>()
    member val Added = add.Publish
    member this.Add i = add.Trigger i
    member val WasReset = reset.Publish
    member this.Reset () = reset.Trigger ()

    member val NameBox = InpcControl<string>("")
    member val NameLabel = InpcControl<string>("")
    member val ScoreInput = InpcControl<int>(0)
    member val ScoreDisplay = InpcControl<string>("")
    member val ScoreDisplay2 = InpcControl<string>("")

type MyEvents =
    | Add of int
    | Reset

[<Test>]
let ``Framework.start and full Sync exercise`` () =
    let mutable multiBindCalls = 0

    let binder (view: MyView) (model: ScoreModel) =
        [ Bind.viewInpc(<@ view.NameBox.Value @>).toModel(<@ model.Name @>)
          Bind.viewInpc(<@ view.ScoreInput.Value @>).toModelOneWay(<@ model.Score @>)
          Bind.model(<@ model.Display @>).toViewInpcOneWay(<@ view.ScoreDisplay.Value @>)
          Bind.model(<@ model.Name @>).toViewInpcOneWay(<@ view.NameLabel.Value @>)
          Bind.modelMulti(<@ model.Name, model.Score, model.Display @>).toFunc(fun (name, score, disp) ->
            multiBindCalls <- multiBindCalls + 1
            view.ScoreDisplay2.Value <- sprintf "%s scored %i" name score
          )
        ]

    let events (view: MyView) =
        [ view.Added |> Observable.map Add
          view.WasReset |> Observable.mapTo Reset
        ]

    let dispatcher = function
        | Add i -> Sync (fun m -> { m with Score = m.Score + i })
        | Reset -> Sync (fun _ -> ScoreModel.Default)

    let view = MyView()
    let model, sub = Framework.start binder events dispatcher view ScoreModel.Default
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
    let binder (view: MyView) (model: ScoreModel) =
        [ Bind.viewInpc(<@ view.NameBox.Value @>).toModel(<@ model.Name @>, id, toView)
        ]

    let events _ = []
    let dispatcher _ = Sync id

    let view = MyView()
    let model, sub = Framework.start binder events dispatcher view ScoreModel.Default
    use __ = sub

    bindingSetView |> shouldEqual 1
    view.NameBox.Value <- "Chad"
    bindingSetView |> shouldEqual 1

[<Test>]
let ``Model-to-view bindings fire in the order they are given`` () =
    let mutable triggered = []
    let binder (view: MyView) (model: ScoreModel) =
        [ Bind.model(<@ model.Score @>).toFunc(fun _ -> triggered <- triggered @ ["Score"])
          Bind.model(<@ model.Name @>).toFunc(fun _ -> triggered <- triggered @ ["Name"])
        ]

    let events (view: MyView) =
        [ view.WasReset |> Observable.mapTo Reset ]

    let dispatcher = function
        | Reset -> Sync (fun _ -> ScoreModel.Default)
        | _ -> Sync id

    let view = MyView()
    let model, sub = Framework.start binder events dispatcher view { Name = "Chad"; Score = 2 }
    use __ = sub
    triggered |> shouldEqual ["Score"; "Name"]

    triggered <- []
    view.Reset()
    triggered |> shouldEqual ["Score"; "Name"]

[<Test>]
let ``View properties that change other properties bound to model don't overwrite a change`` () =
    let binder (view: MyView) (model: ScoreModel) =
        // hook up changes to one view property to affect another
        // a more realistic scenario is a ListBox's DataSource affecting its own SelectedItem
        view.ScoreDisplay.ValueChanged.Add (fun _ -> view.NameLabel.Value <- view.ScoreDisplay.Value)

        [ Bind.model(<@ model.Score @>).toFunc(fun s -> view.ScoreDisplay.Value <- string s)
          Bind.viewInpc(<@ view.NameLabel.Value @>).toModel(<@ model.Name @>)
        ]

    let events (view: MyView) =
        [ view.Added |> Observable.map Add ]

    let add i model =
        let newScore = model.Score + i
        { Score = newScore; Name = "Score: " + (string newScore) }

    let dispatcher = function
        | Add i -> Sync (add i)
        | Reset -> Sync id

    let view = MyView()
    let model, sub = Framework.start binder events dispatcher view { Score = 2; Name = "Score: 2" }
    use __ = sub
    view.ScoreDisplay.Value |> shouldEqual "2"
    view.NameLabel.Value |> shouldEqual "Score: 2"

    view.Add 1
    view.ScoreDisplay.Value |> shouldEqual "3"
    view.NameLabel.Value |> shouldEqual "Score: 3"
    // make sure that the binding to Name does not overwrite our change to the model
    model.Value |> shouldEqual { Score = 3; Name = "Score: 3" }


[<Test>]
let ``Framework.start and exercise nested model properties`` () =
    let mutable homeValues = []
    let binder (view: MyView) (model: ScoreboardModel) =
        [ Bind.viewInpc(<@ view.NameLabel.Value @>).toModel(<@ model.Home.Name @>)
          Bind.model(<@ model.Home.Score @>).toViewInpcOneWay(<@ view.ScoreDisplay.Value @>, string)
          Bind.viewInpc(<@ view.NameBox.Value @>).toModel(<@ model.Away.Name @>)
          Bind.model(<@ model.Away.Score @>).toViewInpcOneWay(<@ view.ScoreDisplay2.Value @>, string)
          Bind.model(<@ model.Home @>).toFunc(fun h -> homeValues <- h :: homeValues)
        ]

    let events (view: MyView) = [
        view.Added |> Observable.map Add
        view.WasReset |> Observable.mapTo Reset
    ]

    let dispatcher = function
        | Add i -> Sync (fun m ->
            { m with Home = { m.Home with Score = m.Home.Score + i } })
        | Reset -> Sync (fun m ->
            { m with Away = { Name = ""; Score = 0 } })

    let initModel = ScoreboardModel.Default
    let view = MyView()
    let model, sub = Framework.start binder events dispatcher view initModel
    use __ = sub

    view.NameLabel.Value |> shouldEqual initModel.Home.Name
    view.ScoreDisplay.Value |> shouldEqual (initModel.Home.Score |> string)
    view.NameBox.Value |> shouldEqual initModel.Away.Name
    view.ScoreDisplay2.Value |> shouldEqual (initModel.Away.Score |> string)
    homeValues |> shouldEqual [model.Value.Home]

    view.Add 2
    model.Value.Home.Score |> shouldEqual 3
    view.ScoreDisplay.Value |> shouldEqual "3"
    homeValues.Head |> shouldEqual { initModel.Home with Score = 3 }
    homeValues.Length |> shouldEqual 2

    view.Reset ()
    model.Value.Away |> shouldEqual { Name = ""; Score = 0 }
    view.NameBox.Value |> shouldEqual ""
    view.ScoreDisplay2.Value |> shouldEqual "0"
    homeValues.Length |> shouldEqual 2

    view.NameLabel.Value <- "Homer"
    model.Value.Home.Name |> shouldEqual "Homer"
    homeValues.Head |> shouldEqual { Name = "Homer"; Score = 3 }
    homeValues.Length |> shouldEqual 3

    view.NameBox.Value <- "Mesothelieauxma"
    model.Value.Away.Name |> shouldEqual "Mesothelieauxma"
