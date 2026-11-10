# := on labelled columns, whatever their class (plan 0001, decision 2):
# codes widen, labels join, a label assigns its code, and the class,
# variable label and format stay. Each test runs on datakit's, haven's
# and dtaparser's classes, built without haven or dtaparser.

for (cls in CLASSES) {
  lab <- function(x, labels, label = NULL) labelled_as(cls, x, labels, label)

  test_that(paste(":= widens labelled codes and keeps class, labels and letters:", cls), {
    d <- tibble::tibble(g = c(1, 1, 2), v = lab(c(1L, 0L, NA), c(No = 0, Yes = 1), label = "V"))
    e <- d
    e[1, v] := 2.5
    expect_s3_class(e$v, cls)
    expect_identical(vctrs::vec_data(e$v), c(2.5, 0, NA))
    expect_equal(val_labels(e$v), c(No = 0, Yes = 1))
    expect_identical(var_label(e$v), "V")
    if (cls != "dtaparser_labelled") expect_identical(class(e$v)[[length(class(e$v))]], "double")
    f <- d
    f[g == 2, v, by = g] := tagged_na("a")
    expect_identical(na_tag(vctrs::vec_data(f$v)), c(NA, NA, "a"))
    expect_s3_class(f$v, cls)
    h <- d
    h[, w] := lab(c(0, 1, 0), c(No = 0, Yes = 1))
    expect_s3_class(h$w, cls)
    expect_identical(val_label(h$w), c("No", "Yes", "No"))
    # Whole numbers keep integer codes integers.
    k <- d
    k[3, v] := 1
    expect_identical(vctrs::vec_data(k$v), c(1L, 0L, 1L))
  })

  test_that(paste(":= assigns a label's code, widening integer codes for .a to .z:", cls), {
    d <- tibble::tibble(v = lab(c(1L, 0L, NA), c(No = 0, Yes = 1, tagged_na(c(Refused = "r")))))
    e <- d
    e[3, v] := "No"
    expect_identical(vctrs::vec_data(e$v), c(1L, 0L, 0L))
    f <- d
    f[1, v] := "Refused"
    expect_type(vctrs::vec_data(f$v), "double")
    expect_identical(na_tag(vctrs::vec_data(f$v)), c("r", NA, NA))
    expect_s3_class(f$v, cls)
    expect_error(d[1, v] := "Maybe", "not a label")
  })

  test_that(paste(":= joins a labelled value's labels, and stops on a code with two texts:", cls), {
    d <- tibble::tibble(v = lab(c(1, 0), c(No = 0, Yes = 1), label = "V"))
    d[2, v] := lab(tagged_na("d"), tagged_na(c("Don't know" = "d")), label = "other")
    expect_s3_class(d$v, cls)
    expect_identical(names(val_labels(d$v)), c("No", "Yes", "Don't know"))
    expect_identical(var_label(d$v), "V")
    expect_identical(val_label(d$v), c("Yes", "Don't know"))
    expect_error(d[1, v] := lab(1, c(YES = 1)), "code 1 is \"Yes\" in one and \"YES\"")
    expect_error(d[1, v] := lab(tagged_na("d"), tagged_na(c(DK = "d"))), "code .d is \"Don't know\" in one and \"DK\"")
  })

  test_that(paste(":= keeps a display format on labelled and plain columns, codes widened or not:", cls), {
    x <- structure(lab(c(1, 0), c(No = 0, Yes = 1)), format = "%3.0f")
    d <- tibble::tibble(v = x, w = structure(c(1L, 2L), format = "%5.0f"))
    d[1, v] := 2.5
    d[1, w] := 0.5
    d[2, v] := "Yes"
    expect_identical(attr(d$v, "format"), "%3.0f")
    expect_identical(attr(d$w, "format"), "%5.0f")
    expect_identical(vctrs::vec_data(d$v), c(2.5, 1))
  })

  test_that(paste(":= makes a new labelled column, NA on the other rows:", cls), {
    d <- tibble::tibble(x = 1:3)
    d[x > 1, v] := lab(c(1L, 0L), c(No = 0, Yes = 1), label = "V")
    expect_s3_class(d$v, cls)
    expect_identical(vctrs::vec_data(d$v), c(NA, 1L, 0L))
    expect_identical(var_label(d$v), "V")
    d[x == 1, v] := tagged_na("z")
    expect_identical(na_tag(vctrs::vec_data(d$v)), c("z", NA, NA))
  })

  test_that(paste("a plain column given a labelled value takes its codes:", cls), {
    d <- tibble::tibble(x = c(1L, 2L))
    d[1, x] := lab(2.5, c(Half = 2.5))
    expect_identical(d$x, c(2.5, 2))
  })

  test_that(paste("by groups a labelled column by its codes, letters apart:", cls), {
    a <- tagged_na("a")
    d <- tibble::tibble(h = lab(c(1, a, a, NA), c(One = 1, tagged_na(c(DK = "a")))))
    d[, n, by = h] := .N
    expect_identical(d$n, c(1L, 2L, 2L, 1L))
  })
}

test_that("unclassed codes with a labels attribute are labelled too", {
  d <- tibble::tibble(v = structure(c(1L, 0L), labels = c(No = 0L, Yes = 1L), label = "V"))
  d[1, v] := "No"
  expect_identical(d$v, structure(c(0L, 0L), labels = c(No = 0L, Yes = 1L), label = "V"))
  d[2, v] := tagged_na("a")
  expect_identical(attributes(d$v), list(labels = c(No = 0, Yes = 1), label = "V"))
  expect_identical(na_tag(unclass(d$v)), c(NA, "a"))
})
