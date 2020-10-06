# Contacts Manager

This VinylUI example uses WPF on .NET Core 3 to build an editable phone book. It has two windows, demonstrates
display and editing of records using text boxes, combo boxes and data grids. It also has an example of text box
validation.

## Project Structure

- ContactsManagerDomain (F#): Contains the type definitions for our models and value converter.
- ContactsManagerUI (C#): Contains the XAML files for the WPF windows. Depends on the domain for the value converter.
- ContactsManager (F#): Contains the entry point and application logic. Depends on the other two projects.

The previous version of this application was a .NET Framework WPF app in a single project that used FsXAML to provide
types from the XAML files. Unfortunately, FsXAML still does not support .NET Core (at time of writing - Oct 2020).
Still, it is desirable that this app serve as a .NET Core example, so this project was upgraded, discarding FsXAML.

The XAML has been moved to a C# library project to use the C# compiler to provide types from the XAML. In order to
use domain types and specifically the IValueConverter in the XAML, it was necessary to move the domain to a third
project so that both other projects could see those types.
