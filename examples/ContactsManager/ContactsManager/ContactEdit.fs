module ContactEdit

open System
open System.Windows
open System.Windows.Controls
open System.Collections.ObjectModel
open FSharp.Control
open VinylUI
open VinylUI.Wpf
open Domain

/// Mutable version of ContactNumber for use in the editable DataGrid
type EditNumber() =
    member val Number = "" with get, set
    member val Type = Other "" with get, set

    member this.ToContactNumber () =
        { Number = this.Number; Type = this.Type }

    static member ofContactNumber cn =
        EditNumber(Number = cn.Number, Type = cn.Type)

/// Model for the Contact Edit window's state
type Model = {
    FirstName: Result<string, string>
    LastName: Result<string, string>
    Groups: string list
    Group: string option
    Numbers: EditNumber ObservableCollection
    SelectedNumberIndex: int option
    Notes: string
    Original: Contact option
    Result: Contact option
}

/// Events for Contact Edit
type Events =
    | MoveNumberUp
    | MoveNumberDown
    | Save
    | Closing of cancel: (unit -> unit)

/// Helper for creating error values
module Errors =
    let nameBlank typ = Error <| sprintf "%s name cannot be blank" typ

/// View type (window) for Contact Edit
type View = FsXaml.XAML<"ContactEditWindow.xaml">

/// View binder function where we create bindings between window controls and model properties.
/// We also do other view setup here.
let private binder (view: View) model =
    // Set the grid source to our mutable number list. We don't need a binding since we're using mutation.
    view.NumberGrid.ItemsSource <- model.Numbers

    // When delete is pressed on number grid, show confirmation prompt
    view.NumberGrid.PreviewKeyDown.Add <| fun e ->
        if e.Key = Input.Key.Delete && e.OriginalSource :? DataGridCell then
            let confirm = MessageBox.Show("Are you sure you want to delete this number?",
                                          "Delete?", MessageBoxButton.YesNo) = MessageBoxResult.Yes
            if not confirm then
                e.Handled <- true

    // Set up number column in grid to disallow entry of non-digit characters
    view.NumberGrid.PreparingCellForEdit.Add <| fun e ->
        if string e.Column.Header = "Number" then
            let textBox = e.EditingElement :?> TextBox
            textBox.PreviewTextInput.Add <| fun e ->
                if e.Text |> Seq.exists (not << Char.IsDigit) then
                    e.Handled <- true

    // Enable number move up/down buttons when movement is possible
    view.NumberGrid.SelectionChanged.Add <| fun _ ->
        if view.NumberGrid.SelectedItems.Count = 1 then
            let index = view.NumberGrid.SelectedIndex
            let count = view.NumberGrid.Items.Count - 1
            view.MoveNumberUp.IsEnabled <- index > 0 && index < count
            view.MoveNumberDown.IsEnabled <- index < count - 1

    // Close the form when the cancel button is clicked
    view.CancelButton.Click.Add <| fun _ -> view.Close()

    // Validation helper for bindings below
    let nonEmpty typ s =
        if String.IsNullOrWhiteSpace s then Errors.nameBlank typ
        else Ok s

    // Create the list of bindings
    [
        // Bind the first/last name text boxes two-way to the model with validation using the Result type.
        // The input is valid if non-empty, otherwise the model property is set to an Error value.
        Bind.view(<@ view.FirstNameBox.Text @>).toModelResult(<@ model.FirstName @>, nonEmpty "First")
        Bind.view(<@ view.LastNameBox.Text @>).toModelResult(<@ model.LastName @>, nonEmpty "Last")

        // Bind the number grid's selected index two-way to the model, converting negative values to None
        Bind.view(<@ view.NumberGrid.SelectedIndex @>).toModel(<@ model.SelectedNumberIndex @>,
                                                               (fun i -> if i >= 0 then Some i else None),
                                                               Option.defaultValue -1)

        // Bind the model's group list one-way to the combo box's item source
        Bind.model(<@ model.Groups @>).toItemsSourceDirect(view.GroupCombo)

        // Bind the group combo's text two-way to the model group.
        // Note that the model property is string option. This overload of toModel automatically converts
        // empty string/whitespace from the view to None on the model and vice-versa.
        Bind.view(<@ view.GroupCombo.Text @>).toModel(<@ model.Group @>)

        // Bind the notes text box directly to the model property
        Bind.view(<@ view.NotesBox.Text @>).toModel(<@ model.Notes @>)
    ]

/// Maps events from view controls to the Events we defined
let private events (view: View) =
    // Create the list of event observables
    [
        // Map button clicks to corresponding events
        view.MoveNumberUp.Click |> Observable.mapTo MoveNumberUp
        view.MoveNumberDown.Click |> Observable.mapTo MoveNumberDown
        view.SaveButton.Click |> Observable.mapTo Save

        // Map the window's closing event to our closing event, passing function that cancels the event when invoked.
        view.Closing |> Observable.map (fun e -> Closing (fun () -> e.Cancel <- true))
    ]

// Move number handler
let private moveNumber dir model =
    match model.SelectedNumberIndex with
    // If a number was selected...
    | Some index ->
        // Swap the numbers in the list
        let temp = model.Numbers.[index]
        model.Numbers.[index] <- model.Numbers.[index + dir]
        model.Numbers.[index + dir] <- temp

        // Return a new model with the new selected index to keep the same number selected after the move
        { model with SelectedNumberIndex = Some (index + dir) }

    // If no number was selected, don't change anything - return the same model
    | None -> model

// Validate the info in the model into a Result of either a valid contact or an error message
let private validate model =
    match model.FirstName, model.LastName with
    | Ok first, Ok last ->
        Ok {
            FirstName = first
            LastName = last
            Group = model.Group
            Numbers = model.Numbers |> Seq.map (fun n -> n.ToContactNumber()) |> Seq.toList
            Notes = model.Notes
        }
    | Error e, _ | _, Error e -> Error e

// Save event handler
let private save close model =
    match validate model with
    | Ok contact ->
        // If the model has a valid contact, close the window
        close ()

        // Return a new model with the contact as the result and as the "original" to prevent the unsaved changes dialog
        { model with
            Result = Some contact
            Original = Some contact
        }

    // If the model has a validation error...
    | Error e ->
        // Show the error
        MessageBox.Show(e, "Invalid Information", MessageBoxButton.OK, MessageBoxImage.Warning) |> ignore
        // Return the same model
        model

// Closing event handler
let private closing cancel model =
    match model.Original, validate model with
    // If we are editing a contact and the model has the same information as the original, don't do anything
    | Some orig, Ok newContact when newContact = orig -> ()
    | _ ->
        // Otherwise, we have unsaved information on the form. Confirm with the user
        let answer = MessageBox.Show("You have unsaved changes! Are you sure you want to close?",
                                     "Discard Changes?", MessageBoxButton.OKCancel, MessageBoxImage.Warning)
        if answer = MessageBoxResult.Cancel then
            // If the user chose to cancel, cancel the event
            cancel ()

    // Always return the same model - this handler is only for side-effects
    model

// Event dispatcher to delegate events to the handlers above.
// Each handler is a function that takes a model and returns a new model.
// We use partial application to provide dependencies and other input.
let private dispatcher (close: unit -> unit) = function
    | MoveNumberUp -> Sync (moveNumber -1)
    | MoveNumberDown -> Sync (moveNumber 1)
    | Save -> Sync (save close)
    | Closing cancel -> Sync (closing cancel)

// Start function to pass into Run, Show, or ShowDialog for the Edit window.
let start groups (contact: Contact option) (view: View) =
    // Create our initial state
    let model =
        match contact with
        | Some c ->
            // If a contact to edit was given, use its info for our initial state
            { FirstName = Ok c.FirstName
              LastName = Ok c.LastName
              Groups = groups
              Group = c.Group
              Numbers = c.Numbers |> List.map EditNumber.ofContactNumber |> ObservableCollection
              SelectedNumberIndex = None
              Notes = c.Notes
              Original = Some c
              Result = None
            }
        | None ->
            // If no contact was given, we're creating a new one. Create a blank initial state
            { FirstName = Errors.nameBlank "First"
              LastName = Errors.nameBlank "Last"
              Groups = groups
              Group = None
              Numbers = ObservableCollection()
              SelectedNumberIndex = None
              Notes = ""
              Original = None
              Result = None
            }

    // Call the Framework start function, providing the binder, events, and dispatcher
    Framework.start binder events (dispatcher view.Close) view model
