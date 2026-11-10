# The path a do-file translated by do2r takes: a reader gives labelled
# columns, datakit's := recodes them, a writer saves a snapshot, and the
# snapshot reads back as the data was. haven reads and writes here;
# dtaparser's read_dta() and save_dta() are checked in CI's dtatools
# job (tools/do2r-dtaparser.R), as datakit does not depend on dtaparser.

test_that("haven's read_dta, then :=, then haven's write_dta reads back the same", {
  skip_if_not_installed("haven")
  src <- tempfile(fileext = ".dta")
  smoker <- haven::labelled(c(1, 0, haven::tagged_na("d"), NA),
                            stats::setNames(c(0, 1, haven::tagged_na("d")), c("No", "Yes", "Don't know")),
                            label = "Smokes daily")
  haven::write_dta(tibble::tibble(
    x = c(1, 2, 3, 4),
    smoker = smoker,
    seen = as.Date(c("2024-01-15", "2024-02-01", NA, "2023-12-31"))
  ), src)
  d <- haven::read_dta(src)
  d[x > 1, y] := 1
  d[x == 4, smoker] := "Don't know"
  d[seen < as.Date("2024-01-20"), early] := 1
  d[x == 1, smoker] := tagged_na("z")
  expect_identical(d$y, c(NA, 1, 1, 1))
  expect_s3_class(d$smoker, "haven_labelled")
  expect_identical(na_tag(vctrs::vec_data(d$smoker)), c("z", NA, "d", "d"))

  out <- tempfile(fileext = ".dta")
  haven::write_dta(d, out)
  back <- haven::read_dta(out)
  # haven gives the new columns a display format, format.stata.
  for (v in c("y", "early")) attr(back[[v]], "format.stata") <- NULL
  expect_identical(back, d)
  expect_s3_class(back$seen, "Date")
  expect_identical(as.character(haven::as_factor(back$smoker)), c(NA, "No", "Don't know", "Don't know"))
  expect_identical(var_label(back$smoker), "Smokes daily")
  expect_identical(back$early, c(1, NA, NA, 1))
})
