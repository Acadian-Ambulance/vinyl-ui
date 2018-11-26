namespace ShapeArea.Forms {
    partial class ShapeAreaForm {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing) {
            if (disposing && (components != null)) {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent() {
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.width = new System.Windows.Forms.TextBox();
            this.height = new System.Windows.Forms.TextBox();
            this.rectangle = new System.Windows.Forms.RadioButton();
            this.ellipse = new System.Windows.Forms.RadioButton();
            this.label4 = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.area = new System.Windows.Forms.Label();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(9, 22);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(38, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Width:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 48);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(41, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Height:";
            // 
            // width
            // 
            this.width.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.width.Location = new System.Drawing.Point(53, 19);
            this.width.Name = "width";
            this.width.Size = new System.Drawing.Size(95, 20);
            this.width.TabIndex = 1;
            // 
            // height
            // 
            this.height.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.height.Location = new System.Drawing.Point(53, 45);
            this.height.Name = "height";
            this.height.Size = new System.Drawing.Size(95, 20);
            this.height.TabIndex = 3;
            // 
            // rectangle
            // 
            this.rectangle.AutoSize = true;
            this.rectangle.Checked = true;
            this.rectangle.Location = new System.Drawing.Point(6, 20);
            this.rectangle.Name = "rectangle";
            this.rectangle.Size = new System.Drawing.Size(74, 17);
            this.rectangle.TabIndex = 0;
            this.rectangle.TabStop = true;
            this.rectangle.Text = "Rectangle";
            this.rectangle.UseVisualStyleBackColor = true;
            // 
            // ellipse
            // 
            this.ellipse.AutoSize = true;
            this.ellipse.Location = new System.Drawing.Point(6, 46);
            this.ellipse.Name = "ellipse";
            this.ellipse.Size = new System.Drawing.Size(55, 17);
            this.ellipse.TabIndex = 1;
            this.ellipse.Text = "Ellipse";
            this.ellipse.UseVisualStyleBackColor = true;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(22, 104);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(37, 13);
            this.label4.TabIndex = 2;
            this.label4.Text = "Area:";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Controls.Add(this.width);
            this.groupBox1.Controls.Add(this.height);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(154, 80);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Dimensions";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.rectangle);
            this.groupBox2.Controls.Add(this.ellipse);
            this.groupBox2.Location = new System.Drawing.Point(172, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(118, 80);
            this.groupBox2.TabIndex = 1;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Shape";
            // 
            // area
            // 
            this.area.AutoSize = true;
            this.area.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.area.Location = new System.Drawing.Point(65, 104);
            this.area.Name = "area";
            this.area.Size = new System.Drawing.Size(52, 13);
            this.area.TabIndex = 3;
            this.area.Text = "<result>";
            // 
            // ShapeAreaForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(302, 131);
            this.Controls.Add(this.area);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.label4);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Name = "ShapeAreaForm";
            this.Text = "Shape Area";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox width;
        private System.Windows.Forms.TextBox height;
        private System.Windows.Forms.RadioButton rectangle;
        private System.Windows.Forms.RadioButton ellipse;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Label area;
    }
}

