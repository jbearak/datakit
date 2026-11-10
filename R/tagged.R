#' Extended missing values `.a` to `.z`
#'
#' A double keeps `.a` to `.z` as `NA` with the letter stored in its
#' bits, as haven's tagged NAs and dtaparser's do, so R sees `NA`.
#' `na_tag()` gives the letters back; `is_tagged_na()` says which values
#' are `.a` to `.z`, or one of the letters in `tag`. `tagged_na()` makes
#' them.
#'
#' These are haven's names, and give haven's values: a tagged NA made by
#' one reads the same in the other. With haven attached as well, the
#' package attached last is the one R finds, and either gives the same
#' letters. Where they differ: haven's `na_tag()` stops on anything that
#' is not a double and drops names, where this one gives `NA` for an
#' integer, logical or factor (none can hold a letter) and keeps names;
#' haven's `is_tagged_na()` takes one `tag`, this one several;
#' haven's `tagged_na()` drops names and makes a tag of any character,
#' where this one keeps names, so that `tagged_na(c(Refused = "r"))`
#' gives a labels entry, and takes only `"a"` to `"z"`, since `"ab"`,
#' `"A"` or `NA` is more likely a typo.
#'
#' @param x For `na_tag()` and `is_tagged_na()`, a double, integer,
#'   logical or factor vector, labelled or not.
#' @param tag Letters `"a"` to `"z"`; `NULL` for any of them.
#' @param ... For `tagged_na()`, letters `"a"` to `"z"`: character
#'   vectors, combined with `c()`.
#' @return `na_tag()`: a character vector as long as `x`, keeping its
#'   names: the letter of each `.a` to `.z`, `NA` for numbers and for
#'   `NA`. `is_tagged_na()`: a logical vector as long as `x`.
#'   `tagged_na()`: a double vector, keeping names, each element `NA`
#'   with its letter.
#' @examples
#' x <- c(1, tagged_na("d"), NA)
#' na_tag(x)                   # NA "d" NA
#' is.na(x)                    # FALSE TRUE TRUE
#' is_tagged_na(x, "d")        # FALSE TRUE FALSE
#' tagged_na("a", "b")         # NA NA
#' tagged_na(c(Refused = "r", "Don't know" = "d"))  # Refused = NA, Don't know = NA
#' @export
na_tag <- function(x) {
  if (is.double(x)) {
    out <- .Call(C_dta_na_tag, x)
  } else if (is.integer(x) || is.logical(x) || is.factor(x)) {
    out <- rep(NA_character_, length(x))
  } else {
    stop("x must be a double, integer, logical or factor vector, not ", class(x)[1], call. = FALSE)
  }
  names(out) <- names(x)
  out
}

#' @rdname na_tag
#' @export
is_tagged_na <- function(x, tag = NULL) {
  tags <- na_tag(x)
  if (is.null(tag)) return(unname(!is.na(tags)))
  if (!is.character(tag) || !length(tag) || anyNA(tag) || !all(tag %in% letters)) {
    stop("tag must be letters \"a\" to \"z\"", call. = FALSE)
  }
  unname(tags %in% tag)
}

#' @rdname na_tag
#' @export
tagged_na <- function(...) {
  x <- c(...)
  if (is.null(x)) x <- character()
  if (!is.character(x)) stop("tagged_na() takes letters \"a\" to \"z\", not ", class(x)[1], call. = FALSE)
  bad <- !(x %in% letters)
  if (any(bad)) {
    stop("tagged_na() takes lowercase letters \"a\" to \"z\", not ", encodeString(x[bad][1], quote = "\""), call. = FALSE)
  }
  # NA_real_'s eight bytes, little-endian, once per element; the letter
  # goes in byte 5, which is bits 32 to 39.
  bytes <- rep(as.raw(c(0xa2, 0x07, 0, 0, 0, 0, 0xf0, 0x7f)), length(x))
  bytes[seq(5, by = 8, length.out = length(x))] <- charToRaw(paste(x, collapse = ""))
  out <- readBin(bytes, "double", n = length(x), size = 8, endian = "little")
  names(out) <- names(x)
  out
}

# Each value's place among the missing values: 0 for a number, 1 for NA
# or NaN, 2 to 27 for .a to .z. A plain integer vector.
missing_rank <- function(x) {
  if (is.factor(x) || !(is.double(x) || is.integer(x) || is.logical(x))) {
    stop("x must be a double, integer, logical, Date, POSIXct or labelled vector, not ", class(x)[1], call. = FALSE)
  }
  .Call(C_dta_missing_rank, x)
}
