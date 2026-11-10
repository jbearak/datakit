# The path a do-file translated by do2r takes, with dtaparser: dtaparser
# reads, datakit's := recodes, dtaparser's save_dta() writes a snapshot,
# and the snapshot reads back as the data was. datakit does not depend
# on dtaparser, so this runs in CI's dtatools job, where dtaparser is
# installed, not in R CMD check:
#   Rscript -e 'testthat::test_file("tools/test-do2r-dtaparser.R", package = "datakit", load_package = "installed")'

test_that("dtaparser's read_dta, then :=, then save_dta reads back the same tibble", {
  src <- tempfile(fileext = ".dta")
  dtaparser::save_dta(tibble::tibble(
    x = c(1L, 2L, 3L, 4L),
    smoker = dtaparser::with_labels(c(1, 0, tagged_na("d"), NA),
                                    c(No = 0, Yes = 1, tagged_na(c("Don't know" = "d"))), label = "Smokes daily"),
    seen = as.Date(c("2024-01-15", "2024-02-01", NA, "2023-12-31"))
  ), src)
  d <- dtaparser::read_dta(src)
  d[x > 1, y] := 1
  d[x == 4, smoker] := "Don't know"
  d[seen < as.Date("2024-01-20"), early] := 1L
  expect_identical(d$y, c(NA, 1, 1, 1))
  expect_identical(na_tag(d$smoker), c(NA, NA, "d", "d"))

  out <- tempfile(fileext = ".dta")
  dtaparser::save_dta(d, out)
  back <- dtaparser::read_dta(out)
  expect_identical(back, d)
  expect_s3_class(back$seen, "Date")
  expect_s3_class(back$smoker, "dtaparser_labelled")
  expect_identical(dtaparser::val_label(back$smoker), c("Yes", "No", "Don't know", "Don't know"))
  expect_identical(dtaparser::var_label(back$smoker), "Smokes daily")
  expect_identical(back$early, c(1L, NA, NA, 1L))
})

test_that("dtaparser's labelled integer codes widen under := and with by", {
  d <- tibble::tibble(g = c(1, 1, 2), v = dtaparser::with_labels(c(1L, 0L, NA), c(No = 0, Yes = 1), label = "V"))
  d[g == 2, v, by = g] := tagged_na("a")
  expect_s3_class(d$v, "dtaparser_labelled")
  expect_identical(na_tag(d$v), c(NA, NA, "a"))
  d[, w, by = g] := dtaparser::with_labels(g, if (g[1] == 1) c(One = 1) else c(Two = 2))
  expect_s3_class(d$w, "dtaparser_labelled")
  expect_identical(dtaparser::val_label(d$w), c("One", "One", "Two"))
})

test_that("datakit's and dtaparser's classes combine both ways", {
  a <- with_labels(c(1L, 2L), c(One = 1, Two = 2))
  b <- dtaparser::with_labels(c(2, tagged_na("d")), c(Two = 2, tagged_na(c(DK = "d"))))
  for (both in list(vctrs::vec_c(a, b), vctrs::vec_c(b, a))) {
    expect_s3_class(both, "datakit_labelled")
    expect_identical(names(attr(both, "labels")), c("One", "Two", "DK"))
  }
})
