namespace Domain

/// Types of phone numbers. This gives us a few pre-defined choices as well as allowing custom types with the "Other" case.
type NumberType =
    | Mobile
    | Work
    | Home
    | Fax
    | Other of string
with
    /// Override ToString to give the custom type string or the name of the chosen pre-defined type.
    override this.ToString () =
        match this with
        | Other t -> t
        | _ -> sprintf "%A" this

    /// List of just the pre-defined choices for populating a drop-down.
    static member Choices = [
        Mobile
        Work
        Home
        Fax
    ]

/// Helper module for formatting.
module Format =
    /// format a 7 or 10 digit phone number with a dash before the last 4 digits and parenthesis around the area code.
    let phone (number: string) =
        let insert c i (s: string) =
            s.Substring(0, i) + c + s.Substring(i)
        if number.Length = 10 then
            number |> insert "-" 6 |> insert ") " 3 |> insert "(" 0
        else if number.Length = 7 then
            number |> insert "-" 3
        else number

/// Represents a phone number with a string of digits and a phone number type.
type ContactNumber = {
    Number: string
    Type: NumberType
} with
    override this.ToString () =
        let typ =
            match string this.Type with
            | "" -> ""
            | typ -> sprintf "%s: " typ
        typ + (Format.phone this.Number)

/// Represents a contact with name, numbers and other data. This is the main data type for this application.
type Contact = {
    FirstName: string
    LastName: string
    Group: string option
    Numbers: ContactNumber list
    Notes: string
} with
    member this.FullName =
        sprintf "%s %s" this.FirstName this.LastName

    member this.PrimaryNumber =
        this.Numbers |> List.tryHead |> Option.map (string)


open System.Windows.Data

/// Value converter to enable display and editing of NumberTypes in WPF controls.
type NumberTypeConverter() =
    interface IValueConverter with
        /// Converts a NumberType to a string for display.
        member this.Convert (value, _, _, _) =
            value |> unbox<NumberType> |> string |> box

        /// Converts a string into a NumberType.
        member this.ConvertBack (value, _, _, _) =
            let typ = value :?> string
            // try to find a NumberType case name matching the input
            let case =
                Reflection.FSharpType.GetUnionCases(typeof<NumberType>)
                |> Seq.tryFind (fun c -> c.Name.ToLower() = typ.ToLower())
            match case with
            // if it matches one of the predefines cases, construct it
            | Some c when c.Name <> "Other" -> Reflection.FSharpValue.MakeUnion(c, [||])
            // otherwise, create it as a custom "Other" type
            | _ -> Other typ |> box
