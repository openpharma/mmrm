# Register S3 Method Register S3 method to a generic.

Register S3 Method Register S3 method to a generic.

## Usage

``` r
h_register_s3(pkg, generic, class, envir = parent.frame())
```

## Arguments

- pkg:

  (`string`) name of the package name.

- generic:

  (`string`) name of the generic.

- class:

  (`string`) class name the function want to dispatch.

- envir:

  (`environment`) the location the method is defined.

## Details

This function is adapted from `emmeans:::register_s3_method()`.
