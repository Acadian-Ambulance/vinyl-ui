[<NUnit.Framework.Apartment(System.Threading.ApartmentState.STA)>]
module WpfTests

open System
open System.Windows.Controls
open NUnit.Framework
open FsUnitTyped
open VinylUI
open VinylUI.Wpf
open BindingTestUtil
open System.Windows

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

type NumberBox() =
    inherit TextBox()

    static let valueProperty = DependencyProperty.Register("Value", typeof<Nullable<int>>, typeof<NumberBox>)
    static member ValueProperty = valueProperty

    member this.Value
        with get () = this.GetValue(valueProperty) :?> Nullable<int>
        and set (v: Nullable<int>) = this.SetValue(valueProperty, v)

    override this.ToString() =
        let v = this.Value |> Option.ofNullable |> Option.map string |> Option.defaultValue "<null>"
        sprintf "NumberBox Value=%s Text=%s" v this.Text

type FakeWindow() =
    member val TextBox = new TextBox()
    member val ListBox = new ListBox()
    member val NumberBox = new NumberBox()

let getErrors (control: Control) = 
    Validation.GetErrors(control) |> Seq.map (fun e -> e.ErrorContent :?> string) |> Seq.toList

let updateControl (cp: BindViewPart<Control, _>) =
    let dp = WpfBinding.getDependencyProperty cp.ControlProperty
    let binding = cp.Control.GetBindingExpression(dp) |> Option.ofObj
    binding |> Option.iter (fun b -> b.UpdateSource())

let testViewToModel sourceUpdate viewExpr startVal newVal expectedVal =
    testViewToModel updateControl sourceUpdate viewExpr startVal newVal expectedVal

let testNonViewToModel viewExpr startVal newVal =
    testNonViewToModel updateControl viewExpr startVal newVal

let sourceUpdateModes = [ OnValidation; OnChange ]

// two-way binding

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind matching properties two-way`` sourceUpdate =
    let window = new FakeWindow()
    let viewExpr = <@ window.TextBox.Text @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Name @>, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    binding |> testModelToView viewExpr model.Name "Bob" "Bob"
    binding |> testViewToModel sourceUpdate viewExpr model.Name "Cat" "Cat"

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind nullable to option two-way`` sourceUpdate =
    let window = new FakeWindow()
    let viewExpr = <@ window.NumberBox.Value @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Age @>, sourceUpdate)
    binding |> testModelToView viewExpr (Option.toNullable model.Age) (Some 31) (Nullable 31)
    binding |> testViewToModel sourceUpdate viewExpr model.Age (Nullable 32) (Some 32)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to val type two-way`` sourceUpdate =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Id @>, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.IdProperty
    binding |> testModelToView viewExpr (box model.Id) 3 (box 3)
    binding |> testViewToModel sourceUpdate viewExpr model.Id (box 4) 4

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to ref type two-way`` sourceUpdate =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- ",Dan,John,Matt".Split([|','|])
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Name @>, sourceUpdate)
    binding |> testModelToView viewExpr (box model.Name) "John" (box "John")
    binding |> testViewToModel sourceUpdate viewExpr model.Name (box "Matt") "Matt"

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to val type option two-way`` sourceUpdate =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Age @>, sourceUpdate)
    binding |> testModelToView viewExpr (model.Age |> Option.toNullable |> box) (Some 31) (box 31)
    binding |> testViewToModel sourceUpdate viewExpr model.Age (box 32) (Some 32)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to ref type option two-way`` sourceUpdate =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- ",D,J,M".Split([|','|])
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.NickName @>, sourceUpdate)
    binding |> testModelToView viewExpr (model.NickName |> Option.toObj |> box) (Some "J") (box "J")
    binding |> testViewToModel sourceUpdate viewExpr model.NickName (box "M") (Some "M")

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind string to int two-way with validation`` sourceUpdate =
    let window = new FakeWindow()
    let viewExpr = <@ window.TextBox.Text @>
    let parseError = "must be a valid integer"
    let nonPositiveError = "must be positive"
    let validator s =
        match System.Int32.TryParse(s) with
        | true, i when i > 0 -> Ok i
        | true, _ -> Error nonPositiveError
        | false, _ -> Error parseError
    let binding = Bind.view(viewExpr).toModelResult(<@ model.AgeResult @>, validator, string, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.AgeResultProperty
    let res r : Result<int, string> = r
    binding |> testModelToView viewExpr "30" (Ok 7 |> res) "7"
    binding |> testModelToView viewExpr "7" (Error "test" |> res) "7"
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "abc" (Error parseError)
    getErrors window.TextBox |> shouldEqual [parseError]
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "0" (Error nonPositiveError)
    getErrors window.TextBox |> shouldEqual [nonPositiveError]
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "7" (Ok 7)
    getErrors window.TextBox |> shouldEqual []

// one way to model binding

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind matching properties one way to model`` sourceUpdate =
    let window = new FakeWindow()
    let viewExpr = <@ window.TextBox.Text @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Name @>, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    binding |> testNonModelToView viewExpr "" "Cat"
    binding |> testViewToModel sourceUpdate viewExpr model.Name "Bob" "Bob"

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind nullable to option one way to model`` sourceUpdate =
    let window = new FakeWindow()
    let viewExpr = <@ window.NumberBox.Value @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Age @>, sourceUpdate)
    binding |> testNonModelToView viewExpr (Nullable()) (Some 31)
    binding |> testViewToModel sourceUpdate viewExpr model.Age (Nullable 32) (Some 32)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to val type one way to model`` sourceUpdate =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- [ 0 .. 100 ] |> List.toArray
    window.ListBox.SelectedIndex <- 0
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Id @>, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.IdProperty
    binding |> testNonModelToView viewExpr (box 0) 3
    binding |> testViewToModel sourceUpdate viewExpr model.Id (box 4) 4

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to ref type one way to model`` sourceUpdate =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- ",Dan,John,Matt".Split([|','|])
    window.ListBox.SelectedIndex <- 0
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Name @>, sourceUpdate)
    binding |> testNonModelToView viewExpr (box "") "John"
    binding |> testViewToModel sourceUpdate viewExpr model.Name (box "Matt") "Matt"

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to val type option one way to model`` sourceUpdate =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- [ 0 .. 100 ] |> List.toArray
    window.ListBox.SelectedIndex <- 0
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Age @>, sourceUpdate)
    binding |> testNonModelToView viewExpr (box 0) (Some 31)
    binding |> testViewToModel sourceUpdate viewExpr model.Age (box 32) (Some 32)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to ref type option one way to model`` sourceUpdate =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- ",D,J,M".Split([|','|])
    window.ListBox.SelectedIndex <- 0
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.NickName @>, sourceUpdate)
    binding |> testNonModelToView viewExpr (box "") (Some "J")
    binding |> testViewToModel sourceUpdate viewExpr model.NickName (box "M") (Some "M")

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind string to int one way to model with validation`` sourceUpdate =
    let window = new FakeWindow()
    let viewExpr = <@ window.TextBox.Text @>
    let parseError = "must be a valid integer"
    let nonPositiveError = "must be positive"
    let validator s =
        match System.Int32.TryParse(s) with
        | true, i when i > 0 -> Ok i
        | true, _ -> Error nonPositiveError
        | false, _ -> Error parseError
    let binding = Bind.view(viewExpr).toModelResultOneWay(<@ model.AgeResult @>, validator, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.AgeResultProperty
    binding |> testNonModelToView viewExpr "" (Ok 7)
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "abc" (Error parseError)
    getErrors window.TextBox |> shouldEqual [parseError]
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "0" (Error nonPositiveError)
    getErrors window.TextBox |> shouldEqual [nonPositiveError]
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "7" (Ok 7)
    getErrors window.TextBox |> shouldEqual []

// one way to view binding

[<Test>]
let ``Bind matching properties one way to view`` () =
    let window = new FakeWindow()
    let viewExpr = <@ window.TextBox.Text @>
    let binding = Bind.model(<@ model.Name @>).toViewOneWay(viewExpr)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    binding |> testModelToView viewExpr model.Name "Bob" "Bob"
    binding |> testNonViewToModel viewExpr model.Name "Cat"

[<Test>]
let ``Bind nullable to option one way to view`` () =
    let window = new FakeWindow()
    let viewExpr = <@ window.NumberBox.Value @>
    let binding = Bind.model(<@ model.Age @>).toViewOneWay(viewExpr)
    binding |> testModelToView viewExpr (Option.toNullable model.Age) (Some 31) (Nullable 31)
    binding |> testNonViewToModel viewExpr model.Age (Nullable 32)

[<Test>]
let ``Bind obj to val type one way to view`` () =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.model(<@ model.Id @>).toViewOneWay(viewExpr)
    binding.ModelProperty |> shouldEqual Model.IdProperty
    binding |> testModelToView viewExpr (box model.Id) 3 (box 3)
    binding |> testNonViewToModel viewExpr model.Id (box 4)

[<Test>]
let ``Bind obj to ref type one way to view`` () =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- ",Dan,John,Matt".Split([|','|])
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.model(<@ model.Name @>).toViewOneWay(viewExpr)
    binding |> testModelToView viewExpr (box model.Name) "John" (box "John")
    binding |> testNonViewToModel viewExpr model.Name (box "Matt")

[<Test>]
let ``Bind obj to val type option one way to view`` () =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.model(<@ model.Age @>).toViewOneWay(viewExpr)
    binding |> testModelToView viewExpr (model.Age |> Option.toNullable |> box) (Some 31) (box 31)
    binding |> testNonViewToModel viewExpr model.Age (box 32)

[<Test>]
let ``Bind obj to ref type option one way to view`` () =
    let window = new FakeWindow()
    window.ListBox.ItemsSource <- ",D,J,M".Split([|','|])
    let viewExpr = <@ window.ListBox.SelectedItem @>
    let binding = Bind.model(<@ model.NickName @>).toViewOneWay(viewExpr)
    binding |> testModelToView viewExpr (model.NickName |> Option.toObj |> box) (Some "J") (box "J")
    binding |> testNonViewToModel viewExpr model.NickName (box "M")

// model to items source

[<Test>]
let ``Bind model to ItemsSource`` () =
    let window = new FakeWindow()
    let getList () = window.ListBox.ItemsSource :?> Book seq |> Seq.toList
    let binding = Bind.model(<@ model.Books @>).toItemsSource(window.ListBox, <@ fun b -> b.Id, b.Name @>)
    binding.ModelProperty |> shouldEqual Model.BooksProperty
    getList () |> shouldEqual model.Books

    window.ListBox.SelectedIndex <- 0
    window.ListBox.SelectedItem |> unbox |> shouldEqual model.Books.[0]
    window.ListBox.SelectedValue |> unbox |> shouldEqual model.Books.[0].Id

    let newList = [ { Id = 99; Name = "Dependency Injection" }; model.Books.[0] ]
    binding.SetView (box newList)
    getList () |> shouldEqual newList

    window.ListBox.SelectedIndex |> shouldEqual 1
    window.ListBox.SelectedItem |> unbox |> shouldEqual model.Books.[0]

[<Test>]
let ``fromSeq preserves selection`` () =
    let window = new FakeWindow()
    let setSource : Book seq -> unit = ListSource.fromSeq window.ListBox <@ fun b -> b.Id, b.Name @>
    setSource books
    window.ListBox.SelectedIndex |> shouldEqual -1

    window.ListBox.SelectedIndex <- 0
    setSource (List.rev books)
    window.ListBox.SelectedIndex |> shouldEqual 1

    setSource [ books.[1] ]
    window.ListBox.SelectedIndex |> shouldEqual -1

[<Test>]
let ``fromItems preserves selection`` () =
    let window = new FakeWindow()
    ListSource.fromItems window.ListBox [1; 2; 3]
    window.ListBox.SelectedIndex |> shouldEqual -1

    window.ListBox.SelectedIndex <- 0
    ListSource.fromItems window.ListBox [3; 1; 2]
    window.ListBox.SelectedIndex |> shouldEqual 1

    ListSource.fromItems window.ListBox [3]
    window.ListBox.SelectedIndex |> shouldEqual -1
