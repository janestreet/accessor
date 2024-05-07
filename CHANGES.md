## Release v0.17.0

- `Accessor.add_to_index`
  * Adds a value to the index without altering the value

## Release v0.16.0

- `Accessor.dummy`
  * Represents a value that is never present
  * `get_option dummy` always returns `None`
  * `map dummy` is always a no-op

- `Accessor.disjoint_field_product`
  * Combines two `field` accessors into one accessing both inner values at once
  * Requires input accessors to access disjoint parts of the outer structure

- `Accessor.disjoint_merge`
  * Combines two `many` accessors into one accessing each inner value accessed by each accessor
  * Requires input accessors to access disjoint parts of the outer structure

- Add hash derivation to the definition of `'a Accessor.Index.t`
