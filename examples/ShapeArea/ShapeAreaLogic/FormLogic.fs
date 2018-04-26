module FormLogic

open System
open System.Windows.Forms
open VinylUI
open VinylUI.WinForms

type Shape = Rectangle | Ellipse

type Model = {
    Width: decimal option
    Height: decimal option
    Shape: Shape
    Area: Result<decimal, string>
}

type Events =
    | DimensionChanged
    | ShapeChanged of Shape

type IShapeAreaForm =
    abstract WidthInput: Control
    abstract HeightInput: Control
    abstract AreaDisplay: Control
    abstract RectangleButton: RadioButton
    abstract EllipseButton: RadioButton

let binder (form: IShapeAreaForm) model =
    let parseDecimal s =
        match Decimal.TryParse s with
        | true, d -> Some d
        | false, _ -> None

    let areaDisplay = function
        | Ok (area: decimal) -> string area
        | Error msg -> sprintf "Error - %s" msg

    [ Bind.view(<@ form.WidthInput.Text @>).toModelOneWay(<@ model.Width @>, parseDecimal)
      Bind.view(<@ form.HeightInput.Text @>).toModelOneWay(<@ model.Height @>, parseDecimal)
      Bind.model(<@ model.Area @>).toViewOneWay(<@ form.AreaDisplay.Text @>, areaDisplay)
    ]

let events (form: IShapeAreaForm) =
    [ form.WidthInput.Validated |> Observable.mapTo DimensionChanged
      form.HeightInput.Validated |> Observable.mapTo DimensionChanged
      form.RectangleButton.CheckedChanged
        |> Observable.filter (fun _ -> form.RectangleButton.Checked)
        |> Observable.mapTo (ShapeChanged Rectangle)
      form.EllipseButton.CheckedChanged
        |> Observable.filter (fun _ -> form.EllipseButton.Checked)
        |> Observable.mapTo (ShapeChanged Ellipse)
    ]

let updateArea model =
    let res =
        match model.Width, model.Height, model.Shape with
        | Some w, Some h, Rectangle -> Ok (w * h)
        | Some w, Some h, Ellipse -> Ok ((w * h / 4m) * (decimal Math.PI))
        | None, Some _, _ -> Error "Missing width"
        | Some _, None, _ -> Error "Missing height"
        | None, None, _ -> Error "Missing width and height"
    { model with Area = res }

let shapeChanged newShape model =
    { model with Shape = newShape }
    |> updateArea

let dispatcher = function
    | DimensionChanged -> Sync updateArea
    | ShapeChanged s -> Sync (shapeChanged s)

let start (form: IShapeAreaForm) =
    let model = {
        Width = None
        Height = None
        Shape = Rectangle
        Area = Error "Missing width and height"
    }
    Framework.start binder events dispatcher form model
