using System;
using System.Windows.Forms;
using VinylUI.WinForms;

namespace ShapeArea {
    static class Program {
        /// <summary>
        /// The main entry point for the application.
        /// The Forms are done in C# since F# does have code generation or designer support for Windows Forms.
        /// </summary>
        [STAThread]
        static void Main() {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            new ShapeAreaForm().Run(FormLogic.start);
        }
    }
}
