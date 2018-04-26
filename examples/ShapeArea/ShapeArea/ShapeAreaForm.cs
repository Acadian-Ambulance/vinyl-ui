using System.Windows.Forms;

namespace ShapeArea {
    public partial class ShapeAreaForm : Form, FormLogic.IShapeAreaForm {
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
