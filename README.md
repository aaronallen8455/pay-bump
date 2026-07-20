# pay-bump :money_with_wings:

Let's say you have a web server with a JSON API where the response or request
payload for a group of endpoints share the same basic structure but with each
having additional fields specific to that endpoint. You wish to define a shared
base record in Haskell to represent this shared payload and then be able to
easily extend it to meet the various needs of the API with minimal boilerplate.

This library presents a pattern for doing so without any fancy types or tricks.
You simply define the base payload and the various extensions separately and
then use the `ExtendedWith` data type to join the base with a chosen extension.
`ExtendedWith` has `ToJSON`, `FromJSON`, and `ToSchema` (from `openapi3`) instances
that present the two distinct types as a single JSON entity by merging fields
from the extension object into the base object, including nested objects.

### Basic Example

```haskell
-- The base payload
data Base = Base
  { foo :: Text
  , bar :: Bar
  } deriving Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Some nested object
data Bar = Bar
  { isBar :: Bool
  } deriving Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Extension object
data Ext = Ext
  { bar :: BarExt
  , baz :: Text
  } deriving Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Extension for nested object
data BarExt = BarExt
  { isExt :: Bool
  } deriving Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

-- The payload for some endpoint
type Payload = Base `ExtendedWith` Ext
```

The contract for the resulting JSON object will have this shape:

```json
{
    "foo": "text",
    "bar": {
        "isBar": true,
        "isExt": true
    },
    "baz": "text"
}
```
