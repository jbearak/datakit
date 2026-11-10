# := with by = joins the groups' results without vctrs' combining
# rules (plan 0001, decision 2): each group's values go in its rows and
# their labels are merged, so groups that return different value labels,
# variable labels and formats join without haven or dtaparser loaded.

grouped_code <- function(cls) c(
  paste0("source(", deparse(normalizePath(testthat::test_path("helper.R"))), ")"),
  paste0("cls <- ", deparse(cls)),
  "lab <- function(x, labels, label = NULL) labelled_as(cls, x, labels, label)",
  "d <- tibble::tibble(g = c(1, 2, 1, 3), x = c(1, 2, 3, 4))",
  "d[, v, by = g] := switch(as.character(g[1]),",
  "  '1' = structure(lab(rep(1L, length(g)), c(One = 1), label = 'first'), format = '%8.0g'),",
  "  '2' = structure(lab(tagged_na('a'), tagged_na(c(DK = 'a')), label = 'second'), format = '%9.0g'),",
  "  '3' = lab(3, c(Three = 3, One = 1)))",
  "result <- list(v = d$v, codes = vctrs::vec_data(d$v), tags = na_tag(vctrs::vec_data(d$v)))",
  "e <- tibble::tibble(g = c(1, 2), v = lab(c(1L, 2L), c(One = 1, Two = 2), label = 'old'))",
  "e[, v, by = g] := if (g[1] == 1) lab(5L, c(Five = 5), label = 'five') else 'One'",
  "result$e <- e$v",
  "f <- tibble::tibble(g = c(1, 2))",
  "result$clash <- tryCatch({ f[, v, by = g] := lab(1, if (g[1] == 1) c(One = 1) else c(Uno = 1)); 'no error' },",
  "  error = function(e) conditionMessage(e))",
  "result$letter_clash <- tryCatch({",
  "  f[, v, by = g] := lab(tagged_na('a'), if (g[1] == 1) tagged_na(c(DK = 'a')) else tagged_na(c(Refused = 'a'))); 'no error' },",
  "  error = function(e) conditionMessage(e))"
)

for (cls in CLASSES) {
  test_that(paste("groups with different labels, variable labels and formats join, haven not loaded:", cls), {
    r <- without_haven(grouped_code(cls))
    expect_false(r$haven_loaded)
    expect_identical(class(r$v)[[1]], cls)
    expect_identical(r$codes[c(1, 3, 4)], c(1, 1, 3))
    expect_identical(r$tags, c(NA, "a", NA, NA))
    expect_identical(names(attr(r$v, "labels")), c("One", "Three", "DK"))
    expect_identical(unname(na_tag(as.double(attr(r$v, "labels")))), c(NA, NA, "a"))
    expect_identical(attr(r$v, "label"), "first")
    expect_identical(attr(r$v, "format"), "%8.0g")
    # Into an existing column: its class, variable label and labels stay,
    # the group's labels join, and text in a group is a label's code.
    expect_identical(class(r$e)[[1]], cls)
    expect_identical(vctrs::vec_data(r$e), c(5L, 1L))
    expect_identical(names(attr(r$e, "labels")), c("One", "Two", "Five"))
    expect_identical(attr(r$e, "label"), "old")
    expect_match(r$clash, "code 1 is \"One\" in one and \"Uno\"")
    expect_match(r$letter_clash, "code .a is \"DK\" in one and \"Refused\"")
  })
}

test_that("groups of plain values still join by vctrs' rules", {
  d <- tibble::tibble(g = c(1, 2, 1))
  d[, f, by = g] := factor(if (g[1] == 1) "a" else "b")
  expect_identical(d$f, factor(c("a", "b", "a")))
  d[, when, by = g] := as.Date("2024-01-01") + g
  expect_identical(d$when, as.Date(c("2024-01-02", "2024-01-03", "2024-01-02")))
  d[, n, by = g] := if (g[1] == 1) 1L else 2.5
  expect_identical(d$n, c(1, 2.5, 1))
})

test_that("in this session too, groups' labels merge into a datakit_labelled", {
  d <- tibble::tibble(g = c(1, 2))
  d[, v, by = g] := with_labels(g, if (g[1] == 1) c(One = 1) else c(Two = 2), label = if (g[1] == 2) "two")
  expect_s3_class(d$v, "datakit_labelled")
  expect_identical(val_labels(d$v), c(One = 1, Two = 2))
  expect_identical(var_label(d$v), "two")
})
