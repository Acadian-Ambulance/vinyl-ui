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
    let private equalIgnoreCase a b = String.Equals(a, b, StringComparison.InvariantCultureIgnoreCase)

    let permute (model: 'Model) changes =
        let t = typeof<'Model>
        let ctor = t.GetConstructors().[0]
        let propNameIs name (prop: PropertyInfo) = equalIgnoreCase name prop.Name
        let props = t.GetProperties()
        let getCurrentValue propName = props |> Seq.find (propNameIs propName) |> (fun p -> p.GetValue model)
        let args =
            ctor.GetParameters()
            |> Array.map (fun param ->
                match changes |> Array.tryFind (fst >> propNameIs param.Name) with
                | Some (_, newValue) -> box newValue
                | None -> getCurrentValue param.Name
            )
        ctor.Invoke(args) :?> 'Model

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
