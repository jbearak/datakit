# The tests plan 0001's decision 1 lists: integer codes take .a to .z by
# [<-, if_else() and case_when() with haven loaded and with haven not
# loaded; a datakit_labelled combines with a plain haven_labelled both
# ways; and haven's functions read datakit's columns.

# The same steps, run here (haven loaded) and in a fresh process that
# never loads haven. Each leaves its results in `result`.
widening_code <- c(
  "x <- with_labels(c(1L, 98L, 0L), c(No = 0, Yes = 1, tagged_na(c(DK = 'd'))), label = 'Q')",
  "result <- list()",
  "y <- x; y[y == 98] <- tagged_na('d'); result$bracket <- y",
  "y <- x; y[[2]] <- tagged_na('z'); result$brackets2 <- y",
  "y <- x; y[2] <- 'DK'; result$text <- y",
  "y <- x; y[2] <- 0.5; result$fraction <- y",
  "if (requireNamespace('dplyr', quietly = TRUE)) {",
  "  result$if_else <- dplyr::if_else(x == 98, tagged_na('d'), x)",
  "  result$case_when <- dplyr::case_when(x == 98 ~ tagged_na('d'), x == 0 ~ tagged_na('n'), .default = x)",
  "}",
  "h <- structure(c(5, 1), labels = c(Five = 5, Yes = 1), label = 'H', class = c('haven_labelled', 'vctrs_vctr', 'double'))",
  "result$dk_first <- vctrs::vec_c(x, h)",
  "result$haven_first <- vctrs::vec_c(h, x)",
  "result$c <- c(x, tagged_na('a'))",
  "result$clash <- tryCatch(vctrs::vec_c(x, structure(1, labels = c(YES = 1), class = c('haven_labelled', 'vctrs_vctr', 'double'))),",
  "  error = function(e) conditionMessage(e))"
)

check_widening <- function(result) {
  dk_class <- c("datakit_labelled", "haven_labelled", "vctrs_vctr", "double")
  expect_identical(class(result$bracket), dk_class)
  expect_identical(na_tag(vctrs::vec_data(result$bracket)), c(NA, "d", NA))
  expect_identical(vctrs::vec_data(result$bracket)[c(1, 3)], c(1, 0))
  expect_identical(attr(result$bracket, "label"), "Q")
  expect_identical(na_tag(vctrs::vec_data(result$brackets2)), c(NA, "z", NA))
  expect_identical(na_tag(vctrs::vec_data(result$text)), c(NA, "d", NA))
  expect_identical(vctrs::vec_data(result$fraction), c(1, 0.5, 0))
  if (!is.null(result$if_else)) {
    expect_identical(class(result$if_else), dk_class)
    expect_identical(na_tag(vctrs::vec_data(result$if_else)), c(NA, "d", NA))
    expect_identical(names(attr(result$if_else, "labels")), c("No", "Yes", "DK"))
    expect_identical(class(result$case_when), dk_class)
    expect_identical(na_tag(vctrs::vec_data(result$case_when)), c(NA, "d", "n"))
    expect_identical(vctrs::vec_data(result$case_when)[[1]], 1)
  }
  for (both in list(result$dk_first, result$haven_first)) {
    expect_identical(class(both), dk_class)
    expect_identical(names(attr(both, "labels")), c("No", "Yes", "Five", "DK"))
  }
  expect_identical(vctrs::vec_data(result$dk_first), c(1, 98, 0, 5, 1))
  expect_identical(vctrs::vec_data(result$haven_first), c(5, 1, 1, 98, 0))
  expect_identical(attr(result$dk_first, "label"), "Q")
  expect_identical(attr(result$haven_first, "label"), "H")
  expect_identical(class(result$c), dk_class)
  expect_identical(na_tag(vctrs::vec_data(result$c)), c(NA, NA, NA, "a"))
  expect_match(result$clash, "code 1 is \"Yes\" in one and \"YES\"")
}

test_that("integer codes take .a to .z and fractions, and haven's class combines, with haven not loaded", {
  result <- without_haven(widening_code)
  expect_false(result$haven_loaded)
  if (requireNamespace("dplyr", quietly = TRUE)) expect_false(is.null(result$if_else))
  check_widening(result)
})

test_that("integer codes take .a to .z and fractions, and haven's class combines, with haven loaded", {
  skip_if_not_installed("haven")
  loadNamespace("haven")
  env <- new.env(parent = globalenv())
  eval(parse(text = widening_code), env)
  check_widening(env$result)
})

test_that("haven's tagged NAs are datakit's, bit for bit", {
  skip_if_not_installed("haven")
  expect_identical(tagged_na(letters), haven::tagged_na(letters))
  expect_identical(writeBin(tagged_na("q"), raw()), writeBin(haven::tagged_na("q"), raw()))
  x <- c(1, NA, NaN, haven::tagged_na("a", "z"))
  expect_identical(na_tag(x), haven::na_tag(x))
  expect_identical(is_tagged_na(x), haven::is_tagged_na(x))
  expect_identical(is_tagged_na(x, "z"), haven::is_tagged_na(x, "z"))
})

test_that("haven's functions read datakit's columns", {
  skip_if_not_installed("haven")
  x <- with_labels(c(1, 0, tagged_na("r"), NA, 2), c(No = 0, Yes = 1, tagged_na(c(Refused = "r"))), label = "Wants")
  expect_identical(haven::na_tag(x), c(NA, NA, "r", NA, NA))
  expect_true(haven::is.labelled(x))
  f <- haven::as_factor(x)
  expect_s3_class(f, "factor")
  expect_identical(as.character(f), c("Yes", "No", "Refused", NA, "2"))
  expect_identical(levels(haven::as_factor(x, levels = "labels")), c("No", "Yes", "Refused"))
  z <- haven::zap_labels(x)
  expect_false(is.object(z))
  expect_null(attr(z, "labels"))
  expect_identical(na_tag(z), na_tag(vctrs::vec_data(x)))
  expect_output(haven::print_labels(x), "Refused")
  ints <- with_labels(c(1L, 0L), c(No = 0, Yes = 1))
  expect_identical(as.character(haven::as_factor(ints)), c("Yes", "No"))
  expect_identical(haven::zap_labels(ints), c(1L, 0L))
})

test_that("printing is the same with haven loaded", {
  skip_if_not_installed("haven")
  loadNamespace("haven")
  x <- with_labels(c(1, tagged_na("r")), c(Yes = 1, tagged_na(c(Refused = "r"))), label = "Wants")
  printed <- utils::capture.output(print(x))
  expect_identical(printed[[1]], "<datakit_labelled<double>[2]>: Wants")
  expect_length(printed, 2)
  expect_match(printed[[2]], "NA(r) [Refused]", fixed = TRUE)
})

test_that(":= copies a column without its labels through haven's zap_labels(), keeping its letters", {
  skip_if_not_installed("haven")
  d <- tibble::tibble(
    g = c(1, 1, 2),
    v = with_labels(c(1, tagged_na("r"), 0), c(No = 0, Yes = 1, tagged_na(c(Refused = "r"))), label = "V")
  )
  d[, w] := haven::zap_labels(v)
  expect_false(inherits(d$w, "haven_labelled"))
  expect_null(val_labels(d$w))
  expect_identical(var_label(d$w), "V")  # haven's zap_labels() keeps it
  attr(d$w, "label") <- NULL
  expect_identical(na_tag(d$w), c(NA, "r", NA))
  expect_s3_class(d$v, "datakit_labelled")
  d[g == 2, w, by = g] := 5
  expect_identical(d$w, c(1, tagged_na("r"), 5))
})

test_that("a haven read combines with a datakit column", {
  skip_if_not_installed("haven")
  path <- tempfile(fileext = ".dta")
  haven::write_dta(data.frame(x = haven::labelled(c(1, 2, haven::tagged_na("d")),
                                                  c(Low = 1, High = 2, haven::tagged_na("d")) |>
                                                    stats::setNames(c("Low", "High", "Don't know")))), path)
  h <- haven::read_dta(path)$x
  x <- with_labels(c(2L, 3L), c(High = 2, Top = 3))
  for (both in list(vctrs::vec_c(h, x), vctrs::vec_c(x, h))) {
    expect_s3_class(both, "datakit_labelled")
    expect_identical(names(attr(both, "labels")), c("Low", "High", "Top", "Don't know"))
  }
  skip_if_not_installed("dplyr")
  pooled <- dplyr::bind_rows(tibble::tibble(x = h), tibble::tibble(x = x))$x
  expect_identical(na_tag(pooled), c(NA, NA, "d", NA, NA))
})
