open System.Windows
open System.IO
open VinylUI.Wpf
open Domain
open Newtonsoft.Json

/// Example contacts to start the address book
let exampleContacts = [
    { FirstName = "Alice"
      LastName = "Henderson"
      Group = Some "Coworkers"
      Numbers =
        [ { Number = "5553331234"; Type = Work }
          { Number = "5551236789"; Type = Mobile }
          { Number = "5553339999"; Type = Fax }
        ]
      Notes = "Project Manager"
    }
    { FirstName = "Brett"
      LastName = "Foreman"
      Group = Some "Coworkers"
      Numbers =
        [ { Number = "5553451111"; Type = Mobile }
          { Number = "5553339876"; Type = Work }
        ]
      Notes = "Server Admin"
    }
    { FirstName = "Chuck"
      LastName = "North"
      Group = Some "Friends"
      Numbers =
        [ { Number = "2345558899"; Type = Mobile }
        ]
      Notes = "Met at the local festival in 2012"
    }
]

/// Save a list of contacts to the given file path
let save fileName contacts =
    let json = JsonConvert.SerializeObject(contacts, Formatting.Indented)
    File.WriteAllText(fileName, json)

/// Load a list of contacts from the given file path if it exists.
/// If the file does not exist, return the example contacts.
let load fileName =
    if File.Exists fileName then
        File.ReadAllText fileName
        |> JsonConvert.DeserializeObject<Contact list>
    else
        exampleContacts

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let fileName = "contacts.json"
    let contacts = load fileName

    // Define the edit contact function - open the edit window as a dialog and return the result from its model.
    // It calls the VinylUI overload of ShowDialog, which takes a start function. We're using partial application to
    // supply dependencies.
    // Start functions take a WPF window and return a model signal (an observable with a current value) and a disposable
    // event subscription.
    let editContact groups c = ContactEdit.View().ShowDialog(ContactEdit.start groups c).Result

    // Run the application with the manager window using the VinylUI overload of Run, which takes its start function,
    // again using partial application to supply dependencies.
    let app = Application()
    app.Run(ContactManager.View(), ContactManager.start editContact (save fileName) contacts) |> ignore
    0
