using System.Windows;
using System.Windows.Controls;

namespace ContactsManagerUI {
    public partial class ContactEditWindow : Window {
        public TextBox FirstNameBox => firstNameBox;
        public TextBox LastNameBox => lastNameBox;
        public DataGrid NumberGrid => numberGrid;
        public Button MoveNumberUp => moveNumberUp;
        public Button MoveNumberDown => moveNumberDown;
        public ComboBox GroupCombo => groupCombo;
        public TextBox NotesBox => notesBox;
        public Button SaveButton => saveButton;
        public Button CancelButton => cancelButton;

        public ContactEditWindow() {
            InitializeComponent();
        }
    }
}
