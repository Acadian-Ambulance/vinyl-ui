module FormLogic

open System
open VinylUI
open VinylUI.WinForms
open ShapeArea.Forms

/// Defines the Shape types
type Shape = Rectangle | Ellipse

/// Model for the form's state
type Model = {
    Width: decimal option
    Height: decimal option
    Shape: Shape
    Area: Result<decimal, string>
}

/// Events for the form
type Events =
    | DimensionChanged
    | ShapeChanged of Shape

/// View binder function where we create bindings between form controls and model properties.
let binder (form: ShapeAreaForm) model =
    // Helper function for width and height bindings below
    let parseDecimal s =
        match Decimal.TryParse s with
        | true, d -> Some d
        | false, _ -> None

    // Helper function for area binding below
    let areaDisplay = function
        | Ok (area: decimal) -> string area
        | Error msg -> sprintf "Error - %s" msg

    // Create the list of bindings
    [
        // Bind the width and height text boxes' Text one way to the model properties.
        // Uses the parseDecimal function to convert the text input into a decimal option.
        Bind.view(<@ form.WidthInput.Text @>).toModelOneWay(<@ model.Width @>, parseDecimal)
        Bind.view(<@ form.HeightInput.Text @>).toModelOneWay(<@ model.Height @>, parseDecimal)

        // Bind the Area Result one way to the display label, using areaDisplay to convert it to a string
        Bind.model(<@ model.Area @>).toViewOneWay(<@ form.AreaDisplay.Text @>, areaDisplay)
    ]

/// Maps events from view controls to the Events we defined
let events (form: ShapeAreaForm) =
    // Create the list of event observables
    [
        // Map changes in the width and height to the DimensionChanged event
        form.WidthInput.Validated |> Observable.mapTo DimensionChanged
        form.HeightInput.Validated |> Observable.mapTo DimensionChanged

        // Map the CheckedChanged events, when changing to checked, to the corresponding ShapeChanged event
        form.RectangleButton.CheckedChanged
            |> Observable.filter (fun _ -> form.RectangleButton.Checked)
            |> Observable.mapTo (ShapeChanged Rectangle)
        form.EllipseButton.CheckedChanged
            |> Observable.filter (fun _ -> form.EllipseButton.Checked)
            |> Observable.mapTo (ShapeChanged Ellipse)
    ]

// Returns a new model with an updated Area calculated from the other properties
let updateArea model =
    let res =
        match model.Width, model.Height, model.Shape with
        // if we have both dimensions, return a valid area
        | Some w, Some h, Rectangle -> Ok (w * h)
        | Some w, Some h, Ellipse -> Ok ((w * h / 4m) * (decimal Math.PI))
        // if we are missing one or both dimensions, return an error
        | None, Some _, _ -> Error "Missing width"
        | Some _, None, _ -> Error "Missing height"
        | None, None, _ -> Error "Missing width and height"
    { model with Area = res }

// Returns a new model with the given shape and updated area
let shapeChanged newShape model =
    { model with Shape = newShape }
    |> updateArea

// Event dispatcher to delegate events to the handlers above.
// Each handler is a function that takes a model and returns a new model.
let dispatcher = function
    | DimensionChanged -> Sync updateArea
    | ShapeChanged s -> Sync (shapeChanged s)

// Start function to pass into Run, Show, or ShowDialog for the form
let start (form: ShapeAreaForm) =
    // Define the initial state
    let model = {
        Width = None
        Height = None
        Shape = Rectangle
        Area = Error "Missing width and height"
    }

    // Call the Framework start function, providing the binder, events, and dispatcher
    Framework.start binder events dispatcher form model
