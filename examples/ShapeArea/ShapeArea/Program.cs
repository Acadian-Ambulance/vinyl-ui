﻿using System;
using System.Windows.Forms;
using VinylUI.WinForms;

namespace ShapeArea {
    static class Program {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main() {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            new ShapeAreaForm().Run(FormLogic.start);
        }
    }
}
