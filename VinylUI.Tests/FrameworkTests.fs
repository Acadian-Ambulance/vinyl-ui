module FrameworkTests

open NUnit.Framework
open VinylUI
open FsUnitTyped

type Record = {
    StringProperty: string
    IntProperty: int
}

[<Test>]
let ``permuteModel creates new model with new property value``() =
    let before = { StringProperty = "before"; IntProperty = 7 }
    let after = Model.permute before "StringProperty" "after"
    after |> shouldEqual { StringProperty = "after"; IntProperty = 7 }
