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
    let r = { StringProperty = "nonsense"; IntProperty = 7 }
    let n = Framework.permuteModel r "StringProperty" "test"
    n |> shouldEqual { StringProperty = "test"; IntProperty = 7 }
