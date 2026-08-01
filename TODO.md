If extension field is optional/nullable but base field isn't, all
sub fields of the extension must be made optional.

Likewise, if base field is optional/nullable but extension isn't,
base field becomes required and all base sub fields become nullable/optional.

Add a wrapper type that allows for extensions that may be null
`OptionalObj` = Maybe. Maybe alone would overwrite the object with null

Rename module to Payload.Extend
