module BindingTestUtil

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

let books = [ 
    { Id = 27; Name = "Programming For the Brave and True" }
    { Id = 53; Name = "Something Like That" } 
]

let bookObjs = books |> List.map (fun b -> BookObj(b.Id, b.Name))

let model = {
    Id = 2
    Name = "Dan"
    NickName = Some "D"
    Age = Some 30
    AgeResult = Ok 30
    Books = books
    BookObjs = bookObjs
    BookIndex = -1
    BookSelection = None
    BookValue = None
}


let controlGet cp = cp.ControlProperty.GetValue cp.Control
let controlSet x cp = cp.ControlProperty.SetValue(cp.Control, x)

let onViewChanged (f: _ -> _) binding =
    match binding.ViewChanged with
    | Some vc -> vc.Subscribe f
    | None -> { new System.IDisposable with member __.Dispose () = () }

let testModelToView (viewExpr: Expr<'v>) (startVal: 'v) newVal expectedVal binding =
    let cp = CommonBinding.controlPart viewExpr
    use __ = binding |> onViewChanged (fun _ -> failwith "view should not be updated here")
    controlGet cp :?> 'v |> shouldEqual startVal
    binding.SetView (box newVal)
    controlGet cp :?> 'v |> shouldEqual expectedVal

let testNonModelToView (viewExpr: Expr<'v>) (startVal: 'v) newVal binding =
    let cp = CommonBinding.controlPart viewExpr
    use __ = binding |> onViewChanged (fun _ -> failwith "view should not be updated here")
    controlGet cp :?> 'v |> shouldEqual startVal
    binding.SetView (box newVal)
    controlGet cp :?> 'v |> shouldEqual startVal

let testViewToModel updateControl sourceUpdate (viewExpr: Expr<'v>) startVal (newVal: 'v) expectedVal binding =
    let cp = CommonBinding.controlPart viewExpr
    let mutable fromView = startVal
    binding.SetView (box startVal)
    use __ = binding |> onViewChanged (fun n -> fromView <- n :?> 'm)
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
    use __ = binding |> onViewChanged (fun n -> fromView <- n :?> 'm)
    controlSet newVal cp
    fromView |> shouldEqual expectedVal

let testNonViewToModel updateControl (viewExpr: Expr<'v>) startVal (newVal: 'v) binding =
    let cp = CommonBinding.controlPart viewExpr
    let mutable fromView = startVal
    binding.SetView (box startVal)
    use __ = binding |> onViewChanged (fun n -> fromView <- n :?> 'm)
    controlSet newVal cp
    fromView |> shouldEqual startVal
    updateControl cp
    fromView |> shouldEqual startVal

let testNonViewInpcToModel (viewExpr: Expr<'v>) startVal (newVal: 'v) binding =
    let cp = CommonBinding.controlPart viewExpr
    binding.SetView (box startVal)
    let mutable fromView = startVal
    use __ = binding |> onViewChanged (fun n -> fromView <- n :?> 'm)
    controlSet newVal cp
    fromView |> shouldEqual startVal
