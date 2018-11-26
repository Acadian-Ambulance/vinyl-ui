using System.Windows.Forms;

namespace ShapeArea.Forms {
    public partial class ShapeAreaForm : Form {
        public Control WidthInput => width;
        public Control HeightInput => height;
        public Control AreaDisplay => area;
        public RadioButton RectangleButton => rectangle;
        public RadioButton EllipseButton => ellipse;

        public ShapeAreaForm() {
            InitializeComponent();
        }
    }
}
