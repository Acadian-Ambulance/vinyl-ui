using System.Windows;
using System.Windows.Controls;

namespace ContactsManagerUI {
    public partial class ContactManagerWindow : Window {
        public Button CreateButton => createButton;
        public Button EditButton => editButton;
        public Button DeleteButton => deleteButton;
        public TextBlock NameDisplay => nameDisplay;
        public TextBlock NumbersDisplay => numbersDisplay;
        public DataGrid ContactGrid => contactGrid;

        public ContactManagerWindow() {
            InitializeComponent();
        }
    }
}
