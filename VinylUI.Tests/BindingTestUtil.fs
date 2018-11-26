module BindingTestUtil

open System.ComponentModel
open Microsoft.FSharp.Quotations
open FsUnitTyped
open VinylUI

type Book = {
    Id: int
    Name: string
}

[<AllowNullLiteralAttribute>]
type BookObj(id: int, name: string) =
    member this.Id = id
    member this.Name = name

type Model = {
    Id: int
    Name: string
    NickName: string option
    Age: int option
    AgeResult: Result<int, string>
    Books: Book list
    BookObjs: BookObj list
    BookIndex: int
    BookSelection: BookObj option
    BookValue: int option
}
with
    static member IdProperty = typedefof<Model>.GetProperty("Id")
    static member NameProperty = typedefof<Model>.GetProperty("Name")
    static member AgeProperty = typedefof<Model>.GetProperty("Age")
    static member AgeResultProperty = typedefof<Model>.GetProperty("AgeResult")
    static member BooksProperty = typedefof<Model>.GetProperty("Books")

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

let controlGet cp = cp.ControlProperty.GetValue cp.Control
let controlSet x cp = cp.ControlProperty.SetValue(cp.Control, x)

let testModelToView (viewExpr: Expr<'v>) (startVal: 'v) newVal expectedVal binding =
    let cp = CommonBinding.controlPart viewExpr
    use s = binding.ViewChanged.Subscribe (fun _ -> failwith "view should not be updated here")
    controlGet cp :?> 'v |> shouldEqual startVal
    binding.SetView (box newVal)
    controlGet cp :?> 'v |> shouldEqual expectedVal

let testNonModelToView (viewExpr: Expr<'v>) (startVal: 'v) newVal binding =
    let cp = CommonBinding.controlPart viewExpr
    use s = binding.ViewChanged.Subscribe (fun _ -> failwith "view should not be updated here")
    controlGet cp :?> 'v |> shouldEqual startVal
    binding.SetView (box newVal)
    controlGet cp :?> 'v |> shouldEqual startVal

let testViewToModel updateControl sourceUpdate (viewExpr: Expr<'v>) startVal (newVal: 'v) expectedVal binding =
    let cp = CommonBinding.controlPart viewExpr
    let mutable fromView = startVal
    binding.SetView (box startVal)
    use s = binding.ViewChanged.Subscribe (fun n -> fromView <- n :?> 'm)
    controlSet newVal cp
    match sourceUpdate with
    | OnChange -> fromView |> shouldEqual expectedVal
    | OnValidation -> fromView |> shouldEqual startVal
    updateControl cp
    fromView |> shouldEqual expectedVal

let testViewInpcToModel (viewExpr: Expr<'v>) startVal (newVal: 'v) expectedVal binding =
    let cp = CommonBinding.controlPart viewExpr
    let mutable fromView = startVal
    binding.SetView (box startVal)
    use s = binding.ViewChanged.Subscribe (fun n -> fromView <- n :?> 'm)
    controlSet newVal cp
    fromView |> shouldEqual expectedVal

let testNonViewToModel updateControl (viewExpr: Expr<'v>) startVal (newVal: 'v) binding =
    let cp = CommonBinding.controlPart viewExpr
    let mutable fromView = startVal
    binding.SetView (box startVal)
    use s = binding.ViewChanged.Subscribe (fun n -> fromView <- n :?> 'm)
    controlSet newVal cp
    fromView |> shouldEqual startVal
    updateControl cp
    fromView |> shouldEqual startVal

let testNonViewInpcToModel (viewExpr: Expr<'v>) startVal (newVal: 'v) binding =
    let cp = CommonBinding.controlPart viewExpr
    binding.SetView (box startVal)
    let mutable fromView = startVal
    use s = binding.ViewChanged.Subscribe (fun n -> fromView <- n :?> 'm)
    controlSet newVal cp
    fromView |> shouldEqual startVal
