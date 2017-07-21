module FrameworkTests

open NUnit.Framework
open VinylUI
open FsUnitTyped

type Record = {
    Text: string
    Number: int
}
with
    static member TextProperty = typedefof<Record>.GetProperty("Text")
    static member NumberProperty = typedefof<Record>.GetProperty("Number")

[<Test>]
let ``Model.changes detects single change``() =
    let original = { Text = "before value"; Number = 1 }
    let updated = { original with Number = 7 }
    let changes = Model.changes original updated |> Seq.toList
    changes |> shouldEqual [Record.NumberProperty, box 7]

[<Test>]
let ``Model.changes detects multiple changes``() =
    let original = { Text = "before value"; Number = 1 }
    let updated = { Text = "after value"; Number = 7 }
    let changes = Model.changes original updated |> Seq.toList
    changes |> shouldEqual [Record.TextProperty, box "after value"; Record.NumberProperty, box 7]

[<Test>]
let ``Model.permute creates new model with new property value``() =
    let original = { Text = "before value"; Number = 1 }

    let newString = Model.permute original "Text" "after value"
    newString |> shouldEqual { original with Text = "after value" }

    let newInt = Model.permute original "Number" 7
    newInt |> shouldEqual { original with Number = 7 }
