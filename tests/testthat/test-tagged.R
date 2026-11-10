# na_tag(), is_tagged_na() and tagged_na(), and the internal
# missing_rank() that `by` groups .a to .z apart with, ported from
# dtaparser's test-missing-rank.R: numbers < NA < .a < ... < .z. The
# expected ranks are that order written out.

test_that("numbers rank 0, NA and NaN 1, and .a to .z 2 to 27", {
  expect_identical(missing_rank(c(0, -1.5, 1e300, Inf, -Inf)), rep(0L, 5))
  expect_identical(missing_rank(c(NA, NaN, NA_real_)), c(1L, 1L, 1L))
  expect_identical(missing_rank(tagged_na(letters)), 2:27)
  expect_identical(missing_rank(tagged_na("a")), 2L)
  expect_identical(missing_rank(tagged_na("z")), 27L)
  expect_identical(missing_rank(double()), integer())
})

test_that("a tagged NA from haven ranks by its letter too", {
  skip_if_not_installed("haven")
  expect_identical(missing_rank(haven::tagged_na("a", "m", "z")), c(2L, 14L, 27L))
})

test_that("the result is a plain integer vector, with no names or attributes", {
  x <- c(a = 1, b = NA, c = tagged_na("d"))
  attr(x, "label") <- "A column"
  r <- missing_rank(x)
  expect_identical(r, c(0L, 1L, 5L))
  expect_null(attributes(r))
  m <- matrix(c(1, NA, tagged_na("a"), 2), 2)
  expect_null(attributes(missing_rank(m)))
  expect_identical(missing_rank(m), c(0L, 1L, 2L, 0L))
})

test_that("integers and logicals rank only 0 or 1", {
  expect_identical(missing_rank(c(1L, NA, -3L)), c(0L, 1L, 0L))
  expect_identical(missing_rank(c(TRUE, NA, FALSE)), c(0L, 1L, 0L))
  expect_identical(missing_rank(NA), 1L)
  expect_identical(missing_rank(integer()), integer())
})

test_that("Date and POSIXct rank their missing values, letters included", {
  d <- as.Date(c("2020-01-01", NA))
  expect_identical(missing_rank(d), c(0L, 1L))
  expect_identical(missing_rank(structure(c(0L, NA), class = "Date")), c(0L, 1L))
  t <- as.POSIXct(c("2020-01-01 12:00:00", NA), tz = "UTC")
  expect_identical(missing_rank(t), c(0L, 1L))
  expect_null(attributes(missing_rank(t)))
  expect_identical(missing_rank(structure(c(0, tagged_na("d"), NA), class = "Date")), c(0L, 5L, 1L))
})

test_that("labelled columns rank their codes, .a to .z included", {
  x <- with_labels(c(1, 0, tagged_na("r"), NA, tagged_na("a")),
                   c(No = 0, Yes = 1, tagged_na(c(Refused = "r"))))
  r <- missing_rank(x)
  expect_identical(r, c(0L, 0L, 19L, 1L, 2L))
  expect_null(attributes(r))
  # Comparing the ranks uses no method of the labelled class.
  expect_identical(r > missing_rank(tagged_na("a")), c(FALSE, FALSE, TRUE, FALSE, FALSE))

  i <- with_labels(c(1L, NA, 0L), c(No = 0L, Yes = 1L))
  expect_identical(missing_rank(i), c(0L, 1L, 0L))

  for (cls in CLASSES) {
    read <- labelled_as(cls, c(0, 1, NA, tagged_na("d"), tagged_na("r")), c(No = 0, Yes = 1, tagged_na(c("Don't know" = "d"))))
    expect_identical(missing_rank(read), c(0L, 0L, 1L, 5L, 19L))
  }
})

test_that("character, factor, list and data frame are errors", {
  msg <- "must be a double, integer, logical, Date, POSIXct or labelled vector"
  expect_error(missing_rank(c("a", "b")), paste0(msg, ", not character"))
  expect_error(missing_rank(factor(c("a", NA))), paste0(msg, ", not factor"))
  expect_error(missing_rank(list(1)), paste0(msg, ", not list"))
  expect_error(missing_rank(data.frame(x = 1)), paste0(msg, ", not data.frame"))
  expect_error(missing_rank(NULL), paste0(msg, ", not NULL"))
})

test_that("comparing ranks with a letter follows Stata's order for >, >=, < and <=", {
  # Stata's order, smallest first, with one number standing for all.
  order <- c("number", ".", paste0(".", letters))
  value <- function(key) {
    if (key == "number") 42 else if (key == ".") NA_real_ else tagged_na(substring(key, 2))
  }
  x <- vapply(order, value, 0)
  ops <- list(">" = `>`, ">=" = `>=`, "<" = `<`, "<=" = `<=`)
  for (letter in letters) {
    rhs <- missing_rank(tagged_na(letter))
    at <- match(paste0(".", letter), order)
    for (op in names(ops)) {
      expected <- ops[[op]](seq_along(order), at)
      got <- ops[[op]](missing_rank(x), rhs)
      expect_identical(got, expected, label = paste0("x ", op, " .", letter))
      expect_false(anyNA(got))
    }
  }

  # The same, written out for .b: number, ., .a, .b, .c, .z.
  x <- c(42, NA, tagged_na(c("a", "b", "c", "z")))
  b <- missing_rank(tagged_na("b"))
  expect_identical(missing_rank(x) > b, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_identical(missing_rank(x) >= b, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_identical(missing_rank(x) < b, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(missing_rank(x) <= b, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("na_tag() gives letters, keeps names, and is NA for types that hold none", {
  x <- c(a = 1, b = NA, c = tagged_na("d"), d = NaN)
  expect_identical(na_tag(x), c(a = NA, b = NA, c = "d", d = NA))
  expect_identical(na_tag(c(1L, NA)), c(NA_character_, NA_character_))
  expect_identical(na_tag(c(TRUE, NA)), c(NA_character_, NA_character_))
  expect_identical(na_tag(factor(c("a", NA))), c(NA_character_, NA_character_))
  expect_identical(na_tag(with_labels(c(1, tagged_na("r")), c(Yes = 1))), c(NA, "r"))
  expect_identical(na_tag(structure(c(0, tagged_na("b")), class = "Date")), c(NA, "b"))
  expect_identical(na_tag(double()), character())
  expect_error(na_tag("a"), "must be a double, integer, logical or factor vector, not character")
  expect_error(na_tag(list(1)), "not list")
})

test_that("is_tagged_na() takes any letter, one or several", {
  x <- c(1, NA, tagged_na(c("a", "b", "z")))
  expect_identical(is_tagged_na(x), c(FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_identical(is_tagged_na(x, "b"), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(is_tagged_na(x, c("a", "z")), c(FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_identical(is_tagged_na(c(a = 1L, b = NA)), c(FALSE, FALSE))
  expect_error(is_tagged_na(x, "A"), "tag must be letters")
  expect_error(is_tagged_na(x, NA_character_), "tag must be letters")
  expect_error(is_tagged_na(x, character()), "tag must be letters")
})

test_that("tagged_na() makes NA with a letter, keeping names, from one vector or several", {
  x <- tagged_na("a", "b")
  expect_true(all(is.na(x)))
  expect_false(any(is.nan(x)))
  expect_identical(na_tag(x), c("a", "b"))
  expect_identical(tagged_na(c("a", "b")), x)
  expect_identical(names(tagged_na(c(Refused = "r", "Don't know" = "d"))), c("Refused", "Don't know"))
  expect_identical(tagged_na(), double())
  expect_identical(na_tag(tagged_na(letters)), letters)
  expect_error(tagged_na("A"), "lowercase letters \"a\" to \"z\", not \"A\"")
  expect_error(tagged_na("ab"), "not \"ab\"")
  expect_error(tagged_na(NA_character_), "not NA")
  expect_error(tagged_na(1), "not numeric")
})
