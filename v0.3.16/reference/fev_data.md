# Example Data on FEV1

**\[stable\]**

## Usage

``` r
fev_data
```

## Format

A `tibble` with 800 rows and 7 variables:

- `USUBJID`: subject ID.

- `AVISIT`: visit number.

- `ARMCD`: treatment, `TRT` or `PBO`.

- `RACE`: 3-category race.

- `SEX`: sex.

- `FEV1_BL`: FEV1 at baseline (%).

- `FEV1`: FEV1 at study visits.

- `WEIGHT`: weighting variable.

- `VISITN`: integer order of the visit.

- `VISITN2`: coordinates of the visit for distance calculation.

## Source

This is an artificial dataset.

## Note

Measurements of FEV1 (forced expired volume in one second) is a measure
of how quickly the lungs can be emptied. Low levels of FEV1 may indicate
chronic obstructive pulmonary disease (COPD).
