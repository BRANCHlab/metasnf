# Summary method for class `data_list`

Returns a data list summary (`data.frame` class object) containing
information on components, features, variable types, domains, and
component dimensions.

## Usage

``` r
# S3 method for class 'data_list'
summary(object, scope = "component", ...)
```

## Arguments

- object:

  A `data_list` class object.

- scope:

  The level of detail for the summary. By default, this is set to
  "component", which returns a summary of the data list at the component
  level. Can also be set to "feature", resulting in a summary at the
  feature level.

- ...:

  Other arguments passed to `summary` (not used in this function)

## Value

A `data.frame` class object. If `scope` is "component", each row shows
the name, variable type, domain, and dimensions of each component. If
`scope` is "feature", each row shows the name, variable type, and domain
of each feature.
