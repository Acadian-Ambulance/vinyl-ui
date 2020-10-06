module ContactManager

open System.Windows
open VinylUI
open VinylUI.Wpf
open Domain

/// Model for the Contact Manager window's state
type Model = {
    Contacts: Contact list
    SelectedContact: Contact option
}

/// Events for Contact Manager
type Events =
    | CreateContact
    | EditContact of Contact
    | DeleteContact of Contact

/// View type (window) for Contact Manager
type View = ContactsManagerUI.ContactManagerWindow

/// View binder function where we create bindings between window controls and model properties.
/// We also do other view setup here.
let private binder (view: View) model =
    // Call a VinylUI extension to add FSharp type converters (to handle options)
    view.ContactGrid.AddFSharpConverterToColumns()

    // Create the list of bindings
    [
        // Bind the model's contact list one way to the grid's item source
        Bind.model(<@ model.Contacts @>).toViewOneWay(<@ view.ContactGrid.ItemsSource @>)

        // Bind the grid's selected item two-way to the model's SelectedContact
        Bind.view(<@ view.ContactGrid.SelectedItem @>).toModel(<@ model.SelectedContact @>)

        // Bind a callback to changes in the selected contact
        Bind.model(<@ model.SelectedContact @>).toFunc(fun sel ->
            // Update the display of contact info on the left side of the window
            match sel with
            | Some c ->
                view.NameDisplay.Text <- sprintf "%s %s" c.FirstName c.LastName
                view.NumbersDisplay.Text <- c.Numbers |> List.map string |> String.concat "\n"
            | None ->
                view.NameDisplay.Text <- ""
                view.NumbersDisplay.Text <- ""
            // Enable the edit and delete buttons only when there is a selection
            view.EditButton.IsEnabled <- sel.IsSome
            view.DeleteButton.IsEnabled <- sel.IsSome
        )
    ]

/// Maps events from view controls to the Events we defined
let private events (view: View) =
    // Helper function that returns the currently selected contact
    let selected _ = view.ContactGrid.SelectedItem |> Option.ofObj |> Option.map unbox<Contact>

    // Create the list of event observables
    [
        // Map button clicks to corresponding events
        view.CreateButton.Click |> Observable.mapTo CreateContact
        view.EditButton.Click |> Observable.choose (selected >> Option.map EditContact)
        view.ContactGrid.MouseDoubleClick |> Observable.choose (selected >> Option.map EditContact)
        view.DeleteButton.Click |> Observable.choose (selected >> Option.map DeleteContact)
    ]

// Edit event handler
let private edit editContact saveContacts contact model =
    // Determine the list of currently used groups for the user to choose from
    let groups = model.Contacts |> List.choose (fun c -> c.Group) |> List.distinct |> List.sort

    // Invoke the edit function and match on the result
    match editContact groups contact with
    | Some newContact ->
        // If the user saved the contact, replace the old contact with the new one in the list and re-sort
        let contacts =
            model.Contacts
            |> List.except (contact |> Option.toList)
            |> List.append [newContact]
            |> List.sortBy (fun c -> c.FirstName, c.LastName)

        // Save the updated list to disk
        saveContacts contacts

        // Return a new model with the updated list and the new contact selected
        { model with
            Contacts = contacts
            SelectedContact = Some newContact
        }
    | None ->
        // If the user cancelled, don't change anything - return the same model
        model

// Delete event handler
let private delete saveContacts (contact: Contact) model =
    // Prompt the user for delete confirmation
    let confirmed =
        MessageBox.Show(sprintf "Are you sure you want to delete %s from your contact list?" contact.FullName,
                        "Delete Contact?",
                        MessageBoxButton.YesNo) = MessageBoxResult.Yes
    // If the user confirmed...
    if confirmed then
        // Remove the contact from the list
        let contacts = model.Contacts |> List.except [contact]

        // Save the updated list to disk
        saveContacts contacts

        // Return a new model with the updated list
        { model with Contacts = contacts }

    // If the user cancelled, return the same model
    else model

// Event dispatcher to delegate events to the handlers above.
// Each handler is a function that takes a model and returns a new model.
// We use partial application to provide dependencies and other input.
let private dispatcher editContact saveContacts = function
    | CreateContact -> Sync (edit editContact saveContacts None)
    | EditContact c -> Sync (edit editContact saveContacts (Some c))
    | DeleteContact c -> Sync (delete saveContacts c)

// Start function to pass into Run, Show, or ShowDialog for the Manager window.
let start editContact saveContacts contacts (view: View) =
    // Define the initial state
    let model = {
        Contacts = contacts
        SelectedContact = None
    }

    // Call the Framework start function, providing the binder, events, and dispatcher
    Framework.start binder events (dispatcher editContact saveContacts) view model
