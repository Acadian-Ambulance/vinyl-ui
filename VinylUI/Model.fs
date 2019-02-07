namespace VinylUI

open System
open System.Reflection
open FSharp.Control
open FSharp.Reflection

[<ReferenceEquality>]
type Binding = {
    ModelProperties: PropertyInfo list
    ViewChanged: IObservable<obj>
    SetView: obj -> unit
}

module Model =
    let permute (model: 'Model) changes =
        let t = typeof<'Model>
        let values =
            FSharpType.GetRecordFields(t) |> Array.map (fun field ->
                changes
                |> Array.tryFind (fst >> (=) field)
                |> Option.map snd
                |> Option.defaultWith (fun () -> field.GetValue model)
            )
        FSharpValue.MakeRecord(t, values) :?> 'Model

    let change (prevModel: 'a) (newModel: 'a) (prop: PropertyInfo) =
        match prop.GetValue prevModel, prop.GetValue newModel with
        | prev, new' when new' <> prev -> Some (prop, new')
        | _ -> None

    let diff props prevModel newModel =
        props |> Seq.choose (change prevModel newModel)

    let getTupledValues model (props: PropertyInfo seq) =
        match props |> Seq.map (fun p -> p.GetValue model) |> Seq.toArray with
        | [| value |] -> value
        | values ->
            let types = props |> Seq.map (fun p -> p.PropertyType) |> Seq.toArray
            FSharpValue.MakeTuple(values, FSharpType.MakeTupleType(types))

    let updateView model bindings =
        bindings |> Seq.iter (fun b -> getTupledValues model b.ModelProperties |> b.SetView)
