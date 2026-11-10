# datakit

Data preparation in R. `d[i, name, by = g] := value` makes or changes a
column for the rows `i`, within groups `g`, so each line of a do-file
stays one line of R. Columns with value labels keep them, and keep the
extended missing values `.a` to `.z`, whether or not haven is loaded.

datakit needs neither haven nor dtaparser. It is under construction: see
[the plan](docs/plans/0001-datakit-on-arrow-rs.md).

## Install

```r
remotes::install_github("jbearak/datakit")
```

## `:=`

```r
library(datakit)
d <- tibble::tibble(age = c(25L, 41L, NA, 37L), g = c(1, 1, 2, 2))
d[age > 35, older] := TRUE            # NA age: not selected
d[, n_in_g, by = g] := .N             # bysort g: gen n_in_g = _N
d[age < 30, age] := age + 0.5         # age widens to double
d[, older] := NULL                    # drops older
```

`i`, `by` and the value are evaluated among the columns. `by` keeps `NA`
and each of `.a` to `.z` in groups of their own. Attach datakit after
dplyr, rlang or data.table, which export a `:=` of their own.

## `datakit_by()`

dplyr's grouping, counting and joins put every missing value in one
group. `datakit_by()` adds each column's letter as a column, so they
stay apart:

```r
d <- tibble::tibble(x = c(1, tagged_na("a"), tagged_na("b"), NA, 1))
dplyr::count(d, datakit_by(x))        # 1, .a, .b and NA apart
```

## Labelled columns and haven

`with_labels()` makes a labelled column; `tagged_na()`, `na_tag()` and
`is_tagged_na()` make and read `.a` to `.z`, with the same bits as
haven's.

```r
x <- with_labels(c(1L, 0L, 98L), c(No = 0, Yes = 1, tagged_na(c(DK = "d"))),
                 label = "Wants a baby")
x[x == 98] <- tagged_na("d")          # the integer codes become doubles
dplyr::if_else(x == 0, "Yes", x)      # a label assigns its code
```

Its class is `c("datakit_labelled", "haven_labelled", "vctrs_vctr",
type)`. R tries datakit's rules first, so integer codes become doubles
when they must hold `.a` to `.z` or a fraction, with `[<-`, `if_else()`,
`case_when()` and `c()`, and columns whose labels differ combine, haven
loaded or not. haven's functions (`as_factor()`, `zap_labels()`,
`print_labels()`) find `haven_labelled` and work on it. A column haven
read combines with one of datakit's in either order. With haven attached
too, its `tagged_na()`, `na_tag()` and `is_tagged_na()` and datakit's
mask each other; both give the same letters.

`:=` treats any column with a `labels` attribute as labelled
(datakit's, haven's or dtaparser's class): it keeps the class, changes
the codes, and merges value labels, also across the groups of `by`.

## Coming next

Following the plan: `read_parquet()` and `read_parquet_info()` on
arrow-rs, then `save_parquet()`, then benchmarks and the Parquet docs.
