# Test Whether a Symbol is an Infix Operator

Test Whether a Symbol is an Infix Operator

## Usage

``` r
is_infix(name)
```

## Arguments

- name:

  (`symbol` or `name` or `string`)  
  a possible reference to an infix operator to check.

## Value

A logical indicating whether the name is the name of an infix operator.

    is_infix(as.name("|"))
    is_infix("|")
    is_infix("c")
