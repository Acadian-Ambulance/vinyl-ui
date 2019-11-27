namespace VinylUI

open System
open System.Reflection
open FSharp.Control
open FSharp.Reflection

type PropertyChain(chain: PropertyInfo list) =
    member this.GetValue source =
        let rec getVal (props: PropertyInfo list) source =
            match props with
            | prop :: props -> prop.GetValue source |> getVal props
            | [] -> source
        getVal chain source

    member this.SetValue(source, value) =
        let rec setVal (props: PropertyInfo list) source =
            match props with
            | [prop] -> prop.SetValue(source, value)
            | prop :: props -> prop.GetValue source |> setVal props
            | [] -> failwith "Cannot set value on empty property chain"
        setVal chain source

    member this.Chain = chain

    member this.LastPropertyType = (List.last chain).PropertyType

    member this.Prepend prop = PropertyChain (prop :: chain)
    member this.Append prop = PropertyChain (chain @ [prop])

    override this.Equals other =
        match other with
        | :? PropertyChain as other -> chain = other.Chain
        | _ -> false

    override this.GetHashCode () =
        chain |> List.fold (fun hc p -> hc*17 + p.GetHashCode()) 0

    override this.ToString() =
        chain |> List.map string |> String.concat " -> "

[<ReferenceEquality>]
type Binding = {
    ModelProperties: PropertyChain list
    ViewChanged: IObservable<obj> option
    SetView: obj -> unit
}

module Model =
    let getProps (typ: Type) =
        typ.GetProperties(BindingFlags.Public ||| BindingFlags.Instance)

    let getComputedProps (typ: Type) =
        getProps typ |> Array.except (FSharpType.GetRecordFields typ) |> Array.toList

    let permute (model: 'Model) changes =
        let rec permuteInner record (changes: (PropertyChain * obj) seq) =
            let t = record.GetType()
            let values =
                FSharpType.GetRecordFields(t) |> Array.map (fun field ->
                    let fieldChanges =
                        changes
                        |> Seq.choose (fun (chain, value) ->
                            match chain.Chain with
                            | prop :: props when prop = field -> Some (PropertyChain props, value)
                            | _ -> None
                        )
                        |> Seq.toList
                    match fieldChanges with
                    | [] -> field.GetValue record
                    | [(chain, value)] when List.isEmpty chain.Chain -> value
                    | propChanges -> permuteInner (field.GetValue record) propChanges
                )
            FSharpValue.MakeRecord(t, values)
        permuteInner model changes :?> 'Model

    let rec change before after (prop: PropertyInfo) : (PropertyChain * obj) seq =
        let beforeVal = prop.GetValue before
        let afterVal = prop.GetValue after
        if beforeVal <> afterVal then
            if FSharpType.IsRecord prop.PropertyType then
                getProps prop.PropertyType
                |> Seq.collect (change beforeVal afterVal)
                |> Seq.map (fun (chain, value) -> (chain.Prepend prop, value))
            else
                Seq.singleton (PropertyChain [prop], afterVal)
        else Seq.empty

    let diff props prevModel newModel =
        props |> Seq.collect (change prevModel newModel)

    let getTupledValues model (props: PropertyChain seq) =
        let props = props |> Seq.toArray
        match props |> Array.map (fun p -> p.GetValue model) with
        | [| value |] -> value
        | values ->
            let types = props |> Array.map (fun p -> p.LastPropertyType)
            FSharpValue.MakeTuple(values, FSharpType.MakeTupleType(types))

    let updateView model binding =
        getTupledValues model binding.ModelProperties |> binding.SetView
