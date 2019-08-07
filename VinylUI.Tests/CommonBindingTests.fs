module CommonBindingTests

open System
open NUnit.Framework
open FsUnitTyped
open VinylUI
open BindingTestUtil

// helper tests

[<Test>]
let ``BindingConvert option converters for record option type handles nulls`` () =
    let toOption = BindingConvert.objToOption<Book option> ()
    let fromOption = BindingConvert.objFromOption<Book option> ()
    toOption null |> shouldEqual None
    fromOption None |> shouldEqual null


type FakeView() =
    member val StringControl = InpcControl("")
    member val IntControl = InpcControl(Nullable<int>())


// two-way binding

[<Test>]
let ``Bind matching properties two-way for INotifyPropertyChanged control`` () =
    let view = new FakeView()
    let viewExpr = <@ view.StringControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModel(<@ model.Name @>)
    binding |> testModelToView viewExpr model.Name "Bob" "Bob"
    binding |> testViewInpcToModel viewExpr model.Name "Cat" "Cat"

[<Test>]
let ``Bind nullable to option two-way for INotifyPropertyChanged control`` () =
    let view = new FakeView()
    let viewExpr = <@ view.IntControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModel(<@ model.Age @>)
    binding |> testModelToView viewExpr (Option.toNullable model.Age) (Some 31) (Nullable 31)
    binding |> testViewInpcToModel viewExpr model.Age (Nullable 32) (Some 32)

[<Test>]
let ``Bind string to string option two-way for INotifyPropertyChanged control`` () =
    let view = new FakeView()
    let viewExpr = <@ view.StringControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModel(<@ model.NickName @>)
    binding |> testModelToView viewExpr "D" (Some "Chip Jiggins") ("Chip Jiggins")
    binding |> testViewInpcToModel viewExpr model.NickName (" ") (None)

// one way to model binding

[<Test>]
let ``Bind matching properties one way to model for INotifyPropertyChanged control`` () =
    let view = new FakeView()
    let viewExpr = <@ view.StringControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModelOneWay(<@ model.Name @>)
    binding.ModelProperties |> shouldEqual [Model.NameProperty]
    binding |> testNonModelToView viewExpr "" "Cat"
    binding |> testViewInpcToModel viewExpr model.Name "Bob" "Bob"

[<Test>]
let ``Bind nullable to option one way to model for INotifyPropertyChanged control`` () =
    let view = new FakeView()
    let viewExpr = <@ view.IntControl.Value @>
    let binding = Bind.viewInpc(viewExpr).toModelOneWay(<@ model.Age @>)
    binding |> testNonModelToView viewExpr (Nullable()) (Some 31)
    binding |> testViewInpcToModel viewExpr model.Age (Nullable 32) (Some 32)

// one way to view binding

[<Test>]
let ``Bind matching properties one way to view for INotifyPropertyChanged control`` () =
    let view = new FakeView()
    let viewExpr = <@ view.StringControl.Value @>
    let binding = Bind.model(<@ model.Name @>).toViewInpcOneWay(viewExpr)
    binding.ModelProperties |> shouldEqual [Model.NameProperty]
    binding |> testModelToView viewExpr model.Name "Bob" "Bob"
    binding |> testNonViewInpcToModel viewExpr model.Name "Cat"

[<Test>]
let ``Bind nullable to option one way to view for INotifyPropertyChanged control`` () =
    let view = new FakeView()
    let viewExpr = <@ view.IntControl.Value @>
    let binding = Bind.model(<@ model.Age @>).toViewInpcOneWay(viewExpr)
    binding |> testModelToView viewExpr (Option.toNullable model.Age) (Some 31) (Nullable 31)
    binding |> testNonViewInpcToModel viewExpr model.Age (Nullable 32)

// model to func

[<Test>]
let ``Bind model to func`` () =
    let mutable fVal = None
    let mutable fCount = 0
    let f n =
        fVal <- Some n
        fCount <- fCount + 1
    let binding = Bind.model(<@ model.Name @>).toFunc(f)
    binding.ModelProperties |> shouldEqual [Model.NameProperty]
    use __ = binding |> onViewChanged (fun _ -> failwith "view should not be updated here")
    (fVal, fCount) |> shouldEqual (Some model.Name, 1)
    binding.SetView (box "Bob")
    (fVal, fCount) |> shouldEqual (Some "Bob", 2)
