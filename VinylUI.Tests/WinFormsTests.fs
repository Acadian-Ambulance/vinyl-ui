module WinFormsTests

open System
open System.Windows.Forms
open System.Reflection
open NUnit.Framework
open FsUnitTyped
open VinylUI
open VinylUI.WinForms
open BindingTestUtil

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

// view stuff

type NumberBox() =
    inherit TextBox()

    let changedEvent = Event<EventHandler, EventArgs>()

    [<CLIEvent>]
    member this.ValueChanged = changedEvent.Publish

    member this.Value
        with get () =
            match System.Int32.TryParse(this.Text) with
            | true, v -> Nullable v
            | _ -> Nullable()
        and set (v: Nullable<int>) =
            this.Text <- string v
            changedEvent.Trigger(this, EventArgs.Empty)

    override this.ToString() =
        let v = this.Value |> Option.ofNullable |> Option.map string |> Option.defaultValue "<null>"
        sprintf "NumberBox Value=%s Text=%s" v this.Text

type FakeForm() =
    let ctx = BindingContext()
    let init (ctl: 'c when 'c :> Control) =
        ctl.BindingContext <- ctx
        ctl.CreateControl()
        ctl

    member val TextBox = new TextBox() |> init
    member val ListBox = new ListBox() |> init
    member val NumberBox = new NumberBox() |> init
    member val ComboBox = new ComboBox() |> init
    member val CustomTextControl = InpcControl("")
    member val CustomIntControl = InpcControl(Nullable<int>())

    interface IDisposable with
        member this.Dispose() =
            this.TextBox.Dispose()
            this.ListBox.Dispose()
            this.NumberBox.Dispose()

let updateControl (cp: BindViewPart<Control, _>) =
    let notify = typedefof<Control>.GetMethod("NotifyValidating", BindingFlags.Instance ||| BindingFlags.NonPublic)
    notify.Invoke(cp.Control, null) |> ignore

let testViewToModel sourceUpdate viewExpr startVal newVal expectedVal =
    testViewToModel updateControl sourceUpdate viewExpr startVal newVal expectedVal

let testNonViewToModel viewExpr startVal newVal =
    testNonViewToModel updateControl viewExpr startVal newVal

let sourceUpdateModes = [ OnValidation; OnChange ]

// two-way binding

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind matching properties two-way`` sourceUpdate =
    use form = new FakeForm()
    let viewExpr = <@ form.TextBox.Text @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Name @>, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    binding |> testModelToView viewExpr model.Name "Bob" "Bob"
    binding |> testViewToModel sourceUpdate viewExpr model.Name "Cat" "Cat"

[<Test>]
let ``Bind matching properties two-way for custom control`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.CustomTextControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModel(<@ model.Name @>)
    binding |> testModelToView viewExpr model.Name "Bob" "Bob"
    binding |> testViewInpcToModel viewExpr model.Name "Cat" "Cat"

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind nullable to option two-way`` sourceUpdate =
    use form = new FakeForm()
    let viewExpr = <@ form.NumberBox.Value @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Age @>, sourceUpdate)
    binding |> testModelToView viewExpr (Option.toNullable model.Age) (Some 31) (Nullable 31)
    binding |> testViewToModel sourceUpdate viewExpr model.Age (Nullable 32) (Some 32)

[<Test>]
let ``Bind nullable to option two-way for custom control`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.CustomIntControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModel(<@ model.Age @>)
    binding |> testModelToView viewExpr (Option.toNullable model.Age) (Some 31) (Nullable 31)
    binding |> testViewInpcToModel viewExpr model.Age (Nullable 32) (Some 32)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind string to string option two-way`` sourceUpdate =
    use form = new FakeForm()
    let viewExpr = <@ form.TextBox.Text @>
    let binding = Bind.view(viewExpr).toModel(<@ model.NickName@>, sourceUpdate)
    binding |> testModelToView viewExpr "D" (None) ("")
    binding |> testViewToModel sourceUpdate viewExpr model.NickName (null) (None)

[<Test>]
let ``Bind string to string option two-way for custom control`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.CustomTextControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModel(<@ model.NickName @>)
    binding |> testModelToView viewExpr "D" (Some "Chip Jiggins") ("Chip Jiggins")
    binding |> testViewInpcToModel viewExpr model.NickName (" ") (None)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to val type two-way`` sourceUpdate =
    use form = new FakeForm()
    form.ListBox.DataSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Id @>, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.IdProperty
    binding |> testModelToView viewExpr (box model.Id) 3 (box 3)
    binding |> testViewToModel sourceUpdate viewExpr model.Id (box 4) 4

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to ref type two-way`` sourceUpdate =
    use form = new FakeForm()
    form.ListBox.DataSource <- ",Dan,John,Matt".Split([|','|])
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Name @>, sourceUpdate)
    binding |> testModelToView viewExpr (box model.Name) "John" (box "John")
    binding |> testViewToModel sourceUpdate viewExpr model.Name (box "Matt") "Matt"

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to val type option two-way`` sourceUpdate =
    use form = new FakeForm()
    form.ListBox.DataSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.Age @>, sourceUpdate)
    binding |> testModelToView viewExpr (model.Age |> Option.toNullable |> box) (Some 31) (box 31)
    binding |> testViewToModel sourceUpdate viewExpr model.Age (box 32) (Some 32)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to ref type option two-way`` sourceUpdate =
    use form = new FakeForm()
    form.ListBox.DataSource <- ",D,J,M".Split([|','|])
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.NickName @>, sourceUpdate)
    binding |> testModelToView viewExpr (model.NickName |> Option.toObj |> box) (Some "J") (box "J")
    binding |> testViewToModel sourceUpdate viewExpr model.NickName (box "M") (Some "M")

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind string to int two-way with validation`` sourceUpdate =
    use form = new FakeForm()
    let viewExpr = <@ form.TextBox.Text @>
    let parseError = "must be a valid integer"
    let nonPositiveError = "must be positive"
    let validator s =
        match System.Int32.TryParse(s) with
        | true, i when i > 0 -> Ok i
        | true, _ -> Error nonPositiveError
        | false, _ -> Error parseError
    use errorProvider = new ErrorProvider()
    let binding = Bind.view(viewExpr).toModelResult(<@ model.AgeResult @>, validator, string, errorProvider, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.AgeResultProperty
    let res r : Result<int, string> = r
    binding |> testModelToView viewExpr "30" (Ok 7 |> res) "7"
    binding |> testModelToView viewExpr "7" (Error "test" |> res) "7"
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "abc" (Error parseError)
    errorProvider.GetError form.TextBox |> shouldEqual parseError
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "0" (Error nonPositiveError)
    errorProvider.GetError form.TextBox |> shouldEqual nonPositiveError
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "7" (Ok 7)
    errorProvider.GetError form.TextBox |> shouldEqual ""

// one way to model binding

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind matching properties one way to model`` sourceUpdate =
    use form = new FakeForm()
    let viewExpr = <@ form.TextBox.Text @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Name @>, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    binding |> testNonModelToView viewExpr "" "Cat"
    binding |> testViewToModel sourceUpdate viewExpr model.Name "Bob" "Bob"

[<Test>]
let ``Bind matching properties one way to model for custom control`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.CustomTextControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModelOneWay(<@ model.Name @>)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    binding |> testNonModelToView viewExpr "" "Cat"
    binding |> testViewInpcToModel viewExpr model.Name "Bob" "Bob"

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind nullable to option one way to model`` sourceUpdate =
    use form = new FakeForm()
    let viewExpr = <@ form.NumberBox.Value @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Age @>, sourceUpdate)
    binding |> testNonModelToView viewExpr (Nullable()) (Some 31)
    binding |> testViewToModel sourceUpdate viewExpr model.Age (Nullable 32) (Some 32)

[<Test>]
let ``Bind nullable to option one way to model for custom control`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.CustomIntControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModelOneWay(<@ model.Age @>)
    binding |> testNonModelToView viewExpr (Nullable()) (Some 31)
    binding |> testViewInpcToModel viewExpr model.Age (Nullable 32) (Some 32)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to val type one way to model`` sourceUpdate =
    use form = new FakeForm()
    form.ListBox.DataSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Id @>, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.IdProperty
    binding |> testNonModelToView viewExpr (box 0) 3
    binding |> testViewToModel sourceUpdate viewExpr model.Id (box 4) 4

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to ref type one way to model`` sourceUpdate =
    use form = new FakeForm()
    form.ListBox.DataSource <- ",Dan,John,Matt".Split([|','|])
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Name @>, sourceUpdate)
    binding |> testNonModelToView viewExpr (box "") "John"
    binding |> testViewToModel sourceUpdate viewExpr model.Name (box "Matt") "Matt"

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to val type option one way to model`` sourceUpdate =
    use form = new FakeForm()
    form.ListBox.DataSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.Age @>, sourceUpdate)
    binding |> testNonModelToView viewExpr (box 0) (Some 31)
    binding |> testViewToModel sourceUpdate viewExpr model.Age (box 32) (Some 32)

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind obj to ref type option one way to model`` sourceUpdate =
    use form = new FakeForm()
    form.ListBox.DataSource <- ",D,J,M".Split([|','|])
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.view(viewExpr).toModelOneWay(<@ model.NickName @>, sourceUpdate)
    binding |> testNonModelToView viewExpr (box "") (Some "J")
    binding |> testViewToModel sourceUpdate viewExpr model.NickName (box "M") (Some "M")

[<TestCaseSource("sourceUpdateModes")>]
let ``Bind string to int one way to model with validation`` sourceUpdate =
    use form = new FakeForm()
    let viewExpr = <@ form.TextBox.Text @>
    let parseError = "must be a valid integer"
    let nonPositiveError = "must be positive"
    let validator s =
        match System.Int32.TryParse(s) with
        | true, i when i > 0 -> Ok i
        | true, _ -> Error nonPositiveError
        | false, _ -> Error parseError
    use errorProvider = new ErrorProvider()
    let binding = Bind.view(viewExpr).toModelResultOneWay(<@ model.AgeResult @>, validator, errorProvider, sourceUpdate)
    binding.ModelProperty |> shouldEqual Model.AgeResultProperty
    binding |> testNonModelToView viewExpr "" (Ok 7)
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "abc" (Error parseError)
    errorProvider.GetError form.TextBox |> shouldEqual parseError
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "0" (Error nonPositiveError)
    errorProvider.GetError form.TextBox |> shouldEqual nonPositiveError
    binding |> testViewToModel sourceUpdate viewExpr model.AgeResult "7" (Ok 7)
    errorProvider.GetError form.TextBox |> shouldEqual ""

// one way to view binding

[<Test>]
let ``Bind matching properties one way to view`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.TextBox.Text @>
    let binding = Bind.model(<@ model.Name @>).toViewOneWay(viewExpr)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    binding |> testModelToView viewExpr model.Name "Bob" "Bob"
    binding |> testNonViewToModel viewExpr model.Name "Cat"

[<Test>]
let ``Bind matching properties one way to view for custom control`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.CustomTextControl.Value @>
    let binding = Bind.model(<@ model.Name @>).toViewInpcOneWay(viewExpr)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    binding |> testModelToView viewExpr model.Name "Bob" "Bob"
    binding |> testNonViewInpcToModel viewExpr model.Name "Cat"

[<Test>]
let ``Bind nullable to option one way to view`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.NumberBox.Value @>
    let binding = Bind.model(<@ model.Age @>).toViewOneWay(viewExpr)
    binding |> testModelToView viewExpr (Option.toNullable model.Age) (Some 31) (Nullable 31)
    binding |> testNonViewToModel viewExpr model.Age (Nullable 32)

[<Test>]
let ``Bind nullable to option one way to view for custom control`` () =
    use form = new FakeForm()
    let viewExpr = <@ form.CustomIntControl.Value @>
    let binding = Bind.model(<@ model.Age @>).toViewInpcOneWay(viewExpr)
    binding |> testModelToView viewExpr (Option.toNullable model.Age) (Some 31) (Nullable 31)
    binding |> testNonViewInpcToModel viewExpr model.Age (Nullable 32)

[<Test>]
let ``Bind obj to val type one way to view`` () =
    use form = new FakeForm()
    form.ListBox.DataSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.model(<@ model.Id @>).toViewOneWay(viewExpr)
    binding.ModelProperty |> shouldEqual Model.IdProperty
    binding |> testModelToView viewExpr (box model.Id) 3 (box 3)
    binding |> testNonViewToModel viewExpr model.Id (box 4)

[<Test>]
let ``Bind obj to ref type one way to view`` () =
    use form = new FakeForm()
    form.ListBox.DataSource <- ",Dan,John,Matt".Split([|','|])
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.model(<@ model.Name @>).toViewOneWay(viewExpr)
    binding |> testModelToView viewExpr (box model.Name) "John" (box "John")
    binding |> testNonViewToModel viewExpr model.Name (box "Matt")

[<Test>]
let ``Bind obj to val type option one way to view`` () =
    use form = new FakeForm()
    form.ListBox.DataSource <- [ 0 .. 100 ] |> List.toArray
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.model(<@ model.Age @>).toViewOneWay(viewExpr)
    binding |> testModelToView viewExpr (model.Age |> Option.toNullable |> box) (Some 31) (box 31)
    binding |> testNonViewToModel viewExpr model.Age (box 32)

[<Test>]
let ``Bind obj to ref type option one way to view`` () =
    use form = new FakeForm()
    form.ListBox.DataSource <- ",D,J,M".Split([|','|])
    let viewExpr = <@ form.ListBox.SelectedItem @>
    let binding = Bind.model(<@ model.NickName @>).toViewOneWay(viewExpr)
    binding |> testModelToView viewExpr (model.NickName |> Option.toObj |> box) (Some "J") (box "J")
    binding |> testNonViewToModel viewExpr model.NickName (box "M")

// model to func

[<Test>]
let ``Bind model to func`` () =
    let mutable fVal = None
    let mutable fCount = 0
    let f n =
        fVal <- Some n
        fCount <- fCount + 1
    let binding = Bind.model(<@ model.Name @>).toFunc(f)
    binding.ModelProperty |> shouldEqual Model.NameProperty
    use s = binding.ViewChanged.Subscribe (fun _ -> failwith "view should not be updated here")
    (fVal, fCount) |> shouldEqual (Some model.Name, 1)
    binding.SetView (box "Bob")
    (fVal, fCount) |> shouldEqual (Some "Bob", 2)

// model to data source

[<Test>]
let ``Bind model to data source`` () =
    use form = new FakeForm()
    let getList () = form.ListBox.DataSource :?> Book seq |> Seq.toList
    let binding = Bind.model(<@ model.Books @>).toDataSource(form.ListBox, <@ fun b -> b.Id, b.Name @>)
    binding.ModelProperty |> shouldEqual Model.BooksProperty
    getList () |> shouldEqual model.Books
    form.ListBox.SelectedIndex |> shouldEqual -1

    form.ListBox.SelectedIndex <- 0
    form.ListBox.SelectedItem |> unbox |> shouldEqual books.[0]
    form.ListBox.SelectedValue |> unbox |> shouldEqual books.[0].Id
    form.ListBox.Text |> shouldEqual books.[0].Name

    let newList = [ { Id = 99; Name = "Dependency Injection" }; books.[0] ]
    binding.SetView (box newList)
    getList () |> shouldEqual newList

    form.ListBox.SelectedIndex |> shouldEqual 1
    form.ListBox.SelectedItem |> unbox |> shouldEqual books.[0]

[<Test>]
let ``fromSeq preserves selection`` () =
    use form = new FakeForm()
    let setSource : Book seq -> unit = ListSource.fromSeq form.ListBox <@ fun b -> b.Id, b.Name @>
    setSource books
    form.ListBox.SelectedIndex |> shouldEqual -1

    form.ListBox.SelectedIndex <- 0
    setSource (List.rev books)
    form.ListBox.SelectedIndex |> shouldEqual 1

    setSource [ books.[1] ]
    form.ListBox.SelectedIndex |> shouldEqual -1

[<Test>]
let ``fromItems preserves selection`` () =
    use form = new FakeForm()
    ListSource.fromItems form.ListBox [1; 2; 3]
    form.ListBox.SelectedIndex |> shouldEqual -1

    form.ListBox.SelectedIndex <- 0
    ListSource.fromItems form.ListBox [3; 1; 2]
    form.ListBox.SelectedIndex |> shouldEqual 1

    ListSource.fromItems form.ListBox [3]
    form.ListBox.SelectedIndex |> shouldEqual -1

type ListControls = ListType | ComboType

let listControls = [ ListType; ComboType ]

[<TestCaseSource("listControls")>]
let ``Model to view correctly updates SelectedIndex to -1`` controlType =
    use form = new FakeForm()
    let ctrl =
        match controlType with
        | ComboType -> form.ComboBox :> ListControl
        | ListType -> form.ListBox :> ListControl

    Bind.model(<@ model.Books @>).toDataSource(ctrl, <@ fun b -> b.Id, b.Name @>) |> ignore

    let viewExpr = <@ ctrl.SelectedIndex @>
    let binding = Bind.view(viewExpr).toModel(<@ model.BookIndex @>)
    binding |> testModelToView viewExpr model.BookIndex 1 1
    binding |> testModelToView viewExpr 1 -1 -1

[<Test>]
let ``Model to view correctly updates SelectedItem to null`` () =
    use form = new FakeForm()
    let ctrl = form.ComboBox

    Bind.model(<@ model.BookObjs @>).toDataSource(ctrl, <@ fun b -> b.Id, b.Name @>) |> ignore

    let viewExpr = <@ ctrl.SelectedItem @>
    let binding = Bind.view(viewExpr).toModel(<@ model.BookSelection @>)

    binding |> testModelToView viewExpr (model.BookSelection |> Option.toObj |> box) (Some bookObjs.[1]) (bookObjs.[1] |> box)
    binding |> testModelToView viewExpr (bookObjs.[1] |> box) None (null |> box)
    
[<TestCaseSource("listControls")>]
let ``Model to view correctly updates SelectedValue to null`` controlType =
    use form = new FakeForm()
    let ctrl =
        match controlType with
        | ComboType -> form.ComboBox :> ListControl
        | ListType -> form.ListBox :> ListControl

    Bind.model(<@ model.Books @>).toDataSource(ctrl, <@ fun b -> b.Id, b.Name @>) |> ignore

    let viewExpr = <@ ctrl.SelectedValue @>
    let binding = Bind.view(viewExpr).toModel(<@ model.BookValue @>)

    binding |> testModelToView viewExpr (model.BookValue |> Option.toNullable |> box) (Some bookObjs.[1].Id) (bookObjs.[1].Id |> box)
    binding |> testModelToView viewExpr (bookObjs.[1].Id |> box) None (null |> box)

// helper tests

[<Test>]
let ``BindingConvert option converters for record option type handles nulls`` () =
    let toOption = BindingConvert.objToOption<Book option> ()
    let fromOption = BindingConvert.objFromOption<Book option> ()
    toOption null |> shouldEqual None
    fromOption None |> shouldEqual null
