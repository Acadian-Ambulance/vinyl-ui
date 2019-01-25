# Model
The model represents the core data that you want to display on the user interface. Its design should be concerned
with what data is displayed and entered into the user interface, but not how that data is displayed.

When deciding what to put on your model, consider the following questions:
- What inputs will the user interface have?
- What outputs or data need to be displayed on the user interface?
- What inputs and outputs will this user interface have with other parts of the program?
- Is there any other data or state that the logic will need to work with?

## Recommended Practices
- Prefer domain types over primitives
- Use `option` for data that may or may not be present
- Use `Result<_, _>` for input that needs validation

## Avoid
- Mutable properties or types
    - This framework is designed to work with immutable types. While you _can_ use properties of mutable types, there
      are limitations.
    - Bindings on properties of mutable types will not be triggered. See the
      [Binding section](#binding-to-a-property-of-a-mutable-type) for a workaround.
- Laziness (`seq` properties)
    - Using lazily-calculated types such as sequences often result in incorrect behavior or possibly degraded
      performance since they will be evaluated multiple times.
    - Prefer `list` or `array` instead.

# Events
Events are all of the actions that the user can perform. 

# Binding

## Binding to a Property of a Mutable Type
TODO computed property

TODO hazard: referencing initial model in binding callback

