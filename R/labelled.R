#' Labelled columns
#'
#' A column with value labels is a `datakit_labelled`: its codes, an
#' integer or double vector with `.a` to `.z` as tagged NAs
#' ([tagged_na()]), with the value labels in its `labels` attribute and
#' the variable label in `label`. Its class is
#' `c("datakit_labelled", "haven_labelled", "vctrs_vctr", type)`, made
#' without haven, so haven need not be installed or loaded. R tries
#' datakit's rules first, so they hold whether haven is loaded or not;
#' haven's functions, such as `haven::as_factor()`, `zap_labels()` and
#' `print_labels()`, find `haven_labelled` further down and work on it.
#' `with_labels()` makes one.
#'
#' **Missing values.** Every missing value, `NA` and `.a` to `.z` alike,
#' is `NA` to `is.na()`, comparisons and arithmetic, as R's rules say.
#' [na_tag()] gives the letters. `unique()`, `count()` and `group_by()`
#' put every missing value in one group, as for any double; group with
#' [datakit_by()] to keep the letters apart.
#'
#' **Text and factors.** `as.character(x)` gives each value's label, or
#' its number where it has none, and `NA` for every missing value.
#' `factor(x)` and `table(x)` follow from it: levels are the values
#' present, in code order, by their text, and no missing value is a
#' level. Printing shows code and label, `1 [Yes]`, and
#' `NA(r) [Refused]` for a labelled `.r`.
#'
#' **Comparisons and arithmetic.** `x == 1`, `x > 0`, `x %in% c(1, 5)`
#' and `match(1, x)` compare codes. A label stands for its code in `==`
#' and `!=`: `x == "Yes"` is `x == 1`. A label of a missing value is an
#' error there, since `NA` compares as `NA` (write `is_tagged_na(x, "r")`),
#' and so are `<` and `>` with text, since labels have no order.
#' `x %in% c("Yes", "No")` compares codes with text and is `FALSE`
#' throughout. Arithmetic, math and summaries (`x + 1`, `abs(x)`,
#' `sum(x)`, `mean(x)`, `max(x)`, `median(x)`, `quantile(x)`) give plain
#' numbers: a computed number has no label.
#'
#' **Combining.** Two labelled columns combine to one with the union of
#' their labels, and so does a `datakit_labelled` with a `haven_labelled`
#' (one haven read, say) or a `dtaparser_labelled`, in either order; the
#' result is a `datakit_labelled`. A code, or a letter, with different
#' text in the two is an error; relabel first with `with_labels()`. The
#' variable label and display format are the first ones found. A labelled
#' column and plain numbers combine to a labelled column with the
#' labelled side's labels, so `if_else(cond, x, 99)` keeps them. Those
#' labels still describe the old codes: `case_when(x == 1 ~ 2, .default =
#' x)` leaves code 2 with code 2's old label. Base `ifelse()` drops the
#' class and labels; use `dplyr::if_else()`.
#'
#' **Integer codes.** Codes stay integers until they meet a value an
#' integer cannot hold, `.a` to `.z` or a fraction; then they become
#' doubles, as Stata's `replace` promotes a byte to a double. So
#' `x[x == 98] <- tagged_na("d")`, `if_else(x == 98, tagged_na("d"), x)`
#' and `case_when(x == 98 ~ tagged_na("d"), .default = x)` all work on
#' integer codes, with haven loaded or not.
#'
#' **Plain vectors.** `as.numeric(x)` and `vctrs::vec_data(x)` give the
#' codes with no attributes, `.a` to `.z` kept; `unclass(x)` keeps
#' `labels` and `label`. `as.integer(x)` is an error when `x` holds `.a`
#' to `.z` or fractions. A label assigns its code, as in
#' `x[i] <- "No"`, `if_else(cond, "No", x)` or `d[i, x] := "Refused"`; a
#' string that labels no code, or two, is an error.
#'
#' @param x An integer or double vector, or a labelled column
#'   (`datakit_labelled`, `haven_labelled` or `dtaparser_labelled`),
#'   whose codes are kept and whose labels are replaced.
#' @param labels The codes, named by their text: an integer or double
#'   vector with `.a` to `.z` as [tagged_na()], no repeated code or
#'   letter, no plain `NA` or `NaN`, and no empty or `NA` name.
#' @param label The variable label, a string; `NULL` keeps `x`'s.
#' @return A `datakit_labelled` with `x`'s codes. Its labels are kept as
#'   doubles in code order, `.a` to `.z` last.
#' @examples
#' x <- with_labels(c(1L, 0L, NA, 1L),
#'                  c(No = 0, Yes = 1, tagged_na(c(Refused = "r"))),
#'                  label = "Wants a baby")
#' x                   # 1 [Yes] 0 [No] NA 1 [Yes]
#' x == "Yes"          # TRUE FALSE NA TRUE
#' x[3] <- "Refused"   # the codes become doubles to hold .r
#' na_tag(x)           # NA NA "r" NA
#' as.character(x)     # "Yes" "No" NA "Yes"
#'
#' y <- with_labels(c(0, tagged_na("d")), c(No = 0, tagged_na(c("Don't know" = "d"))))
#' z <- vctrs::vec_c(x, y)
#' attr(z, "labels")   # No = 0, Yes = 1, Don't know = NA, Refused = NA
#' @name datakit_labelled
#' @aliases with_labels
#' @export
with_labels <- function(x, labels, label = NULL) {
  if (is.null(label)) label <- attr(x, "label", exact = TRUE)
  format <- attr(x, "format", exact = TRUE)
  if (inherits(x, LABELLED)) x <- vctrs::vec_data(x)
  if (!(is.double(x) || is.integer(x)) || is.object(x)) {
    stop("x must be an integer or double vector, not ", class(x)[1], call. = FALSE)
  }
  if (!is.null(label) && !(is.character(label) && length(label) == 1 && !is.na(label))) {
    stop("label must be a string or NULL", call. = FALSE)
  }
  new_labelled(x, checked_labels(labels), label, format)
}

# The classes whose columns `:=` and the combining rules read through
# their `labels` attribute.
LABELLED <- c("datakit_labelled", "haven_labelled", "dtaparser_labelled")

# Whether x is a labelled column: integer or double codes of one of the
# labelled classes (whose `labels` may be missing, as no labels), or of
# any other class but factor with a `labels` attribute.
is_labelled <- function(x) {
  (is.double(x) || is.integer(x)) && !is.factor(x) &&
    (inherits(x, LABELLED) || !is.null(attr(x, "labels", exact = TRUE)))
}

checked_labels <- function(labels) {
  if (!(is.double(labels) || is.integer(labels)) || is.object(labels)) {
    stop("labels must be an integer or double vector, not ", class(labels)[1], call. = FALSE)
  }
  labels <- stats::setNames(as.double(labels), names(labels))
  text <- names(labels)
  if (length(labels) && (is.null(text) || anyNA(text) || any(!nzchar(text)))) {
    stop("labels must each have a name, its text", call. = FALSE)
  }
  if (any(is.na(labels) & is.na(na_tag(labels)))) stop("labels cannot label NA or NaN, only numbers and .a to .z", call. = FALSE)
  keys <- label_keys(labels)
  if (anyDuplicated(keys)) stop("labels has code ", keys[anyDuplicated(keys)], " more than once", call. = FALSE)
  labels
}

# The class around the codes `x`, with `labels` in code order and the
# display format, if any.
new_labelled <- function(x, labels, label, format = NULL) {
  attributes(x) <- NULL
  vctrs::new_vctr(x, labels = in_code_order(labels), label = label, format = format,
                  class = c("datakit_labelled", "haven_labelled"), inherit_base_type = TRUE)
}

in_code_order <- function(labels) {
  if (is.null(labels)) return(stats::setNames(double(), character()))
  labels <- stats::setNames(as.double(labels), names(labels))
  tags <- na_tag(labels)
  numbers <- labels[is.na(tags)]
  tagged <- labels[!is.na(tags)]
  c(numbers[order(numbers)], tagged[order(tags[!is.na(tags)])])
}

# Each code as text, ".r" for a tagged NA, to match codes by: match()
# pairs every NA with every other.
label_keys <- function(codes) {
  codes <- as.double(codes)
  tags <- na_tag(codes)
  unname(ifelse(is.na(tags), sprintf("%.17g", codes), paste0(".", tags)))
}

labels_of <- function(x) attr(x, "labels", exact = TRUE)
label_of <- function(x) attr(x, "label", exact = TRUE)
format_of <- function(x) attr(x, "format", exact = TRUE)

# The labels `a` and `b` together, `a` as it is when `b` adds nothing,
# else in code order. A code with two texts is an error.
union_labels <- function(a, b) {
  all <- c(stats::setNames(as.double(a), names(a)), stats::setNames(as.double(b), names(b)))
  keys <- label_keys(all)
  first <- !duplicated(keys)
  kept <- all[first]
  text <- names(kept)[match(keys, keys[first])]
  clash <- which(names(all) != text)
  if (length(clash)) {
    i <- clash[[1]]
    stop("Can't combine labels: code ", keys[[i]], " is \"", text[[i]], "\" in one and \"", names(all)[[i]],
         "\" in the other; relabel one with with_labels()", call. = FALSE)
  }
  if (length(kept) == length(a) && !is.null(a)) return(a)
  in_code_order(kept)
}

# The plain codes of a labelled value, other values as they are.
plain <- function(x) if (is_labelled(x)) vctrs::vec_data(x) else x

# `codes` with x's attributes: its class, labels and the rest. Codes
# that became doubles get a class that says so, and double labels.
recoded <- function(x, codes, labels = labels_of(x)) {
  attrs <- attributes(x)
  attrs$names <- names(codes)
  if (is.double(codes) && !is.null(attrs$class)) attrs$class[attrs$class == "integer"] <- "double"
  if (is.double(codes) && is.integer(labels)) labels <- stats::setNames(as.double(labels), names(labels))
  attrs$labels <- labels
  attributes(codes) <- attrs[!vapply(attrs, is.null, NA)]
  codes
}

# The text of each of `values` under the labels of x, NA where a value
# has none. Numbers match by value, .a to .z by letter.
val_label <- function(x, values = x) {
  labels <- labels_of(x)
  values <- as.double(plain(values))
  out <- rep(NA_character_, length(values))
  if (is.null(labels)) return(out)
  codes <- as.double(labels)
  text <- names(labels)
  # match() pairs NA with NA, so NA keys stay out of both sides.
  number <- !is.na(values)
  coded <- !is.na(codes)
  out[number] <- text[coded][match(values[number], codes[coded])]
  tags <- na_tag(values)
  code_tags <- na_tag(codes)
  out[!is.na(tags)] <- text[!is.na(code_tags)][match(tags[!is.na(tags)], code_tags[!is.na(code_tags)])]
  out
}

# The code each string in `text` labels in `labels`: NA for NA. A
# string that labels no code, or two, names no one code and is an error.
text_codes <- function(text, labels) {
  text_of <- names(labels) %||% character()
  hit <- match(text, text_of)
  unknown <- !is.na(text) & is.na(hit)
  if (any(unknown)) {
    shown <- if (length(text_of)) paste0("\"", text_of, "\"", collapse = ", ") else "none"
    stop("\"", text[unknown][[1]], "\" is not a label of this column; its labels are ", shown, call. = FALSE)
  }
  shared <- !is.na(text) & text %in% text_of[duplicated(text_of)]
  if (any(shared)) stop("\"", text[shared][[1]], "\" labels more than one code; give the code", call. = FALSE)
  unname(as.double(labels))[hit]
}

# Whether the doubles `v` are whole numbers an integer holds, with no
# NaN or .a to .z, which only a double keeps.
fits_integer <- function(v) {
  if (!is.double(v)) return(TRUE)
  known <- v[!is.na(v)]
  all(known == trunc(known) & abs(known) <= .Machine$integer.max) && !any(is.nan(v)) &&
    !any(is_tagged_na(v[is.na(v)]))
}

# `x`'s codes as `to`'s type: integer only when no code is a fraction
# or .a to .z, which an integer cannot hold.
codes_as <- function(x, to, x_arg, to_arg, call) {
  x <- plain(x)
  if (is.double(vctrs::vec_data(to))) return(as.double(x))
  out <- vctrs::vec_cast(x, integer(), x_arg = x_arg, call = call)
  if (is.double(x)) {
    out <- vctrs::maybe_lossy_cast(out, x, to, lossy = is_tagged_na(x), x_arg = x_arg, to_arg = to_arg, call = call)
  }
  out
}

# The prototype of two labelled columns combined: datakit's class, the
# union of their labels, integer codes only if both are integers.
labelled_ptype2 <- function(x, y, x_arg, y_arg, call) {
  if (!is_labelled(x) || !is_labelled(y)) vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg, call = call)
  both_integer <- is.integer(vctrs::vec_data(x)) && is.integer(vctrs::vec_data(y))
  new_labelled(if (both_integer) integer() else double(), union_labels(labels_of(x), labels_of(y)),
               label_of(x) %||% label_of(y), format_of(x) %||% format_of(y))
}

# `x` cast to the labelled `to`, keeping `to`'s class. A label `to`
# lacks would be lost, so it is an error.
labelled_cast <- function(x, to, x_arg, to_arg, call) {
  if (!is_labelled(x) || !is_labelled(to)) vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, call = call)
  extra <- setdiff(label_keys(labels_of(x)), label_keys(labels_of(to)))
  if (length(extra)) {
    vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, call = call,
                                  details = paste0("Code ", extra[[1]], " has a label that `", to_arg %|% "to",
                                                   "` lacks, so the cast would drop it."))
  }
  recoded(to, codes_as(x, to, x_arg, to_arg, call))
}

`%|%` <- function(x, y) if (nzchar(x)) x else y

# Codes that an integer cannot hold, from `value` given to x[i] <- value.
needs_double <- function(x, value) {
  if (is.character(value)) value <- text_codes(value, labels_of(x))
  value <- plain(value)
  is.double(value) && !is.object(value) && !fits_integer(value)
}

#' @export
`[<-.datakit_labelled` <- function(x, ..., value) {
  if (is.integer(vctrs::vec_data(x)) && needs_double(x, value)) x <- recoded(x, as.double(vctrs::vec_data(x)))
  NextMethod()
}

#' @export
`[[<-.datakit_labelled` <- function(x, ..., value) {
  if (is.integer(vctrs::vec_data(x)) && needs_double(x, value)) x <- recoded(x, as.double(vctrs::vec_data(x)))
  NextMethod()
}

#' @export
vec_ptype_abbr.datakit_labelled <- function(x, ...) {
  if (is.integer(vctrs::vec_data(x))) "int+lbl" else "dbl+lbl"
}

#' @export
vec_ptype_full.datakit_labelled <- function(x, ...) {
  paste0("datakit_labelled<", typeof(vctrs::vec_data(x)), ">")
}

# The header and footer, so that printing is the same with haven loaded
# or not: the labels are shown beside each value, not again below.
#' @export
obj_print_header.datakit_labelled <- function(x, ...) {
  label <- label_of(x)
  cat("<", vctrs::vec_ptype_full(x), "[", length(x), "]>", if (!is.null(label)) paste0(": ", label), "\n", sep = "")
  invisible(x)
}

#' @export
obj_print_footer.datakit_labelled <- function(x, ...) invisible(x)

#' @export
format.datakit_labelled <- function(x, ...) {
  codes <- vctrs::vec_data(x)
  tags <- na_tag(as.double(codes))
  shown <- ifelse(is.na(codes), ifelse(is.na(tags), "NA", paste0("NA(", tags, ")")), as.character(codes))
  text <- val_label(x)
  ifelse(is.na(text), shown, paste0(shown, " [", text, "]"))
}

#' @export
as.character.datakit_labelled <- function(x, ...) {
  codes <- vctrs::vec_data(x)
  text <- val_label(x)
  out <- ifelse(is.na(text), as.character(codes), text)
  out[is.na(codes)] <- NA_character_
  out
}

#' @export
mtfrm.datakit_labelled <- function(x) vctrs::vec_data(x)

#' @export
vec_ptype2.datakit_labelled.datakit_labelled <- function(x, y, ..., x_arg = "", y_arg = "", call = rlang::caller_env()) {
  labelled_ptype2(x, y, x_arg, y_arg, call)
}
#' @export
vec_ptype2.datakit_labelled.haven_labelled <- vec_ptype2.datakit_labelled.datakit_labelled
#' @export
vec_ptype2.haven_labelled.datakit_labelled <- vec_ptype2.datakit_labelled.datakit_labelled
#' @export
vec_ptype2.datakit_labelled.dtaparser_labelled <- vec_ptype2.datakit_labelled.datakit_labelled
#' @export
vec_ptype2.dtaparser_labelled.datakit_labelled <- vec_ptype2.datakit_labelled.datakit_labelled

#' @export
vec_ptype2.datakit_labelled.double <- function(x, y, ...) new_labelled(double(), labels_of(x), label_of(x), format_of(x))
#' @export
vec_ptype2.double.datakit_labelled <- function(x, y, ...) new_labelled(double(), labels_of(y), label_of(y), format_of(y))

#' @export
vec_ptype2.datakit_labelled.integer <- function(x, y, ...) {
  new_labelled(vctrs::vec_ptype(vctrs::vec_data(x)), labels_of(x), label_of(x), format_of(x))
}
#' @export
vec_ptype2.integer.datakit_labelled <- function(x, y, ...) {
  new_labelled(vctrs::vec_ptype(vctrs::vec_data(y)), labels_of(y), label_of(y), format_of(y))
}

# Text is a label, so the codes it gives may be .a to .z: doubles.
#' @export
vec_ptype2.datakit_labelled.character <- function(x, y, ...) new_labelled(double(), labels_of(x), label_of(x), format_of(x))
#' @export
vec_ptype2.character.datakit_labelled <- function(x, y, ...) new_labelled(double(), labels_of(y), label_of(y), format_of(y))

#' @export
vec_cast.datakit_labelled.datakit_labelled <- function(x, to, ..., x_arg = "", to_arg = "", call = rlang::caller_env()) {
  labelled_cast(x, to, x_arg, to_arg, call)
}
#' @export
vec_cast.datakit_labelled.haven_labelled <- vec_cast.datakit_labelled.datakit_labelled
#' @export
vec_cast.haven_labelled.datakit_labelled <- vec_cast.datakit_labelled.datakit_labelled
#' @export
vec_cast.datakit_labelled.dtaparser_labelled <- vec_cast.datakit_labelled.datakit_labelled
#' @export
vec_cast.dtaparser_labelled.datakit_labelled <- vec_cast.datakit_labelled.datakit_labelled

#' @export
vec_cast.datakit_labelled.double <- function(x, to, ..., x_arg = "", to_arg = "", call = rlang::caller_env()) {
  recoded(to, codes_as(x, to, x_arg, to_arg, call))
}
#' @export
vec_cast.datakit_labelled.integer <- vec_cast.datakit_labelled.double

#' @export
vec_cast.datakit_labelled.character <- function(x, to, ..., x_arg = "", to_arg = "", call = rlang::caller_env()) {
  recoded(to, codes_as(text_codes(x, labels_of(to)), to, x_arg, to_arg, call))
}

#' @export
vec_cast.double.datakit_labelled <- function(x, to, ...) as.double(vctrs::vec_data(x))

#' @export
vec_cast.integer.datakit_labelled <- function(x, to, ..., x_arg = "", to_arg = "", call = rlang::caller_env()) {
  codes_as(x, integer(), x_arg, to_arg, call)
}

#' @export
#' @method vec_arith datakit_labelled
vec_arith.datakit_labelled <- function(op, x, y, ...) {
  codes <- vctrs::vec_data(x)
  # -x and +x arrive with y as vctrs' MISSING().
  if (inherits(y, "MISSING")) {
    return(switch(op, "-" = -codes, "+" = codes, vctrs::stop_incompatible_op(op, x, y)))
  }
  vctrs::vec_arith_base(op, codes, plain(y))
}

#' @export
#' @method vec_arith.numeric datakit_labelled
vec_arith.numeric.datakit_labelled <- function(op, x, y, ...) vctrs::vec_arith_base(op, x, vctrs::vec_data(y))

#' @export
vec_math.datakit_labelled <- function(.fn, .x, ...) vctrs::vec_math_base(.fn, vctrs::vec_data(.x), ...)

#' @export
Summary.datakit_labelled <- function(..., na.rm = FALSE) {
  do.call(.Generic, c(lapply(list(...), plain), na.rm = na.rm))
}

#' @export
median.datakit_labelled <- function(x, na.rm = FALSE, ...) stats::median(vctrs::vec_data(x), na.rm = na.rm, ...)

#' @export
quantile.datakit_labelled <- function(x, ...) stats::quantile(vctrs::vec_data(x), ...)

#' @export
summary.datakit_labelled <- function(object, ...) summary(vctrs::vec_data(object), ...)

# == and != with a string compare the code it labels. A label of a
# missing value would compare as NA on every row, and labels have no
# order, so those are errors. Everything else goes on to vctrs.
#' @export
Ops.datakit_labelled <- function(e1, e2) {
  if (nargs() == 2 && (is.character(e1) || is.character(e2)) && .Generic %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (!.Generic %in% c("==", "!=")) {
      stop("`", .Generic, "`: labels have no order; compare codes, as in x ", .Generic, " 1", call. = FALSE)
    }
    labelled <- if (is.character(e1)) e2 else e1
    text <- if (is.character(e1)) e1 else e2
    codes <- text_codes(text, labels_of(labelled))
    missing <- !is.na(text) & is.na(codes)
    if (any(missing)) {
      tag <- na_tag(codes[missing][[1]])
      stop("\"", text[missing][[1]], "\" labels the missing value .", tag, ", and NA compares as NA; write is_tagged_na(x, \"",
           tag, "\")", call. = FALSE)
    }
    return(get(.Generic)(vctrs::vec_data(labelled), codes))
  }
  NextMethod()
}
