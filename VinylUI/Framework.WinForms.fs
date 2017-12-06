namespace VinylUI.WinForms

open System.Reflection
open System.Windows.Forms
open VinylUI

type WinBinding = System.Windows.Forms.Binding

module DataBind =
    let private convertUpdateMode = function
        | OnValidation -> DataSourceUpdateMode.OnValidation
        | OnChange -> DataSourceUpdateMode.OnPropertyChanged
        | Never -> DataSourceUpdateMode.Never

    let private getUpdateModeFor (controlProperty: PropertyInfo) updateMode =
        let withDefault =
            match updateMode, controlProperty.Name with
            | Some mode, _ -> mode
            | _, "Checked" -> OnChange
            | _ -> OnValidation
        convertUpdateMode withDefault

    let private addConverter (binding: WinBinding) (converter: BindingConverter<'s, 'c>) =
        let sanitize (ctrlValue: obj) =
            match ctrlValue with
            | :? System.DBNull -> Unchecked.defaultof<'c>
            | value -> value :?> 'c
        binding.Format.Add (fun e -> e.Value <- e.Value :?> 's |> converter.ToControl)
        binding.Parse.Add (fun e -> e.Value <- e.Value |> sanitize |> converter.ToSource)

    let createBinding (bindingInfo: BindingInfo<Control>) =
        let b = bindingInfo
        let bindingUpdateMode = getUpdateModeFor b.ControlProperty b.UpdateMode
        let controlBinding = WinBinding(b.ControlProperty.Name,
                                        b.Source,
                                        b.SourceProperty.Name,
                                        true,
                                        bindingUpdateMode)
        b.Converter |> Option.iter (addConverter controlBinding)

        match b.Control, b.ControlProperty.Name with
        | :? ComboBox as cbo, "SelectedItem" ->
            // workaround for bindings to SelectedItem not triggering until focus is lost
            cbo.SelectedIndexChanged.Add (fun _ -> controlBinding.WriteValue())
        | _ -> ()

        b.Control.DataBindings.Add controlBinding
        controlBinding

    /// Creates bindings from a quotation of statements, where each statement is an assignment to
    /// a view property from a model property or a call to a function passing a model property.
    let fromExpr assignExprs = CommonBinding.fromExpr createBinding assignExprs

    /// temporary helper
    let fromProp source (property: PropertyInfo) (updateView: 'a -> unit) =
        updateView (property.GetValue source :?> 'a)
        let binding = {
            ModelProperty = property
            ViewChanged = Event<_>().Publish
            SetView = (fun o -> updateView (o :?> 'a))
        }
        binding


/// Helpers for setting the DataSource of ListControls
module ListSource =
    open System.Collections.Generic
    open Microsoft.FSharp.Quotations
    open VinylUI.BindingPatterns

    let private setSource (control: ListControl) (source: 'a seq) displayMember valueMember =
        control.DataSource <- null
        (* we have to set the members both before and after setting the DataSource. We have to set it before in case
         * event handlers try to read a value as soon as the DataSource is set, and we have to set it after because 
         * CheckedListBox and DataGridViewComboBoxEditingControl forget their DisplayMember when DataSource is set *)
        control.DisplayMember <- displayMember
        control.ValueMember <- valueMember
        control.DataSource <- List source
        control.DisplayMember <- displayMember
        control.ValueMember <- valueMember
        control.SelectedIndex <- -1

    let fromSeq control (displayValueProperties: Expr<'a -> (_ * _)>) (source: 'a seq) =
        let (displayMember, valueMember) =
            match displayValueProperties with
            | PropertyTupleSelector (displayProp, valueProp) -> (displayProp.Name, valueProp.Name)
            | _ -> failwith (sprintf "Expected an expression that selects a tuple of the display property and value property, but was given %A" displayValueProperties)
        setSource control source displayMember valueMember

    /// Set the DataSource to the contents of a dictionary.
    /// The keys will be the control items' values and the dictionary values will be the items' text.
    let fromDict control (source: IDictionary<'value, 'display>) =
        setSource control source "Value" "Key"

    /// Set the DataSource to a sequence of value * display pairs.
    let fromPairs (control: ListControl) (source: ('value * 'display) seq) = fromDict control (dict source)
