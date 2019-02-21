open System.Windows.Forms
open VinylUI.WinForms
open ShapeArea.Forms

[<EntryPoint>]
[<System.STAThread>]
let main args =
    Application.EnableVisualStyles();
    Application.SetCompatibleTextRenderingDefault(false);

    let form = new ShapeAreaForm()
    form.RunApp(FormLogic.start) |> ignore
    0
