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
