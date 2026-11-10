# Run R code in a fresh R process with this session's library paths;
# returns its output lines. The code goes in a file and R_LIBS through
# the environment, since system2() quotes differently on Windows and
# ignores its `env` there.
fresh_r <- function(code) {
  script <- tempfile(fileext = ".R")
  writeLines(code, script)
  old <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(if (is.na(old)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old))
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(system2(rscript, shQuote(script), stdout = TRUE, stderr = TRUE))
  out[!grepl("^OMP:", out)]
}

# Runs `code` (a character vector of R lines) in a fresh R process
# that loads datakit and not haven. The code leaves its results in
# `result`, a list; they come back with `haven_loaded`, whether haven's
# namespace was loaded at the end (a namespace, once loaded, stays), which
# the tests expect to be FALSE.
without_haven <- function(code) {
  out <- tempfile(fileext = ".rds")
  lines <- fresh_r(c(
    "suppressPackageStartupMessages(library(datakit))",
    code,
    "result$haven_loaded <- 'haven' %in% loadedNamespaces()",
    paste0("saveRDS(result, ", deparse(out), ")")
  ))
  if (!file.exists(out)) stop("the fresh R process failed:\n", paste(lines, collapse = "\n"), call. = FALSE)
  readRDS(out)
}

# Evaluates `code` from the global environment, as user code runs, so
# that S3 methods come from NAMESPACE's registrations; the tests
# themselves run where the package's internal functions are visible.
as_user <- function(code, ...) eval(substitute(code), list(...), globalenv())

# The accessors the tests read columns with; datakit does not export
# them (dtaparser and labelled do).
val_labels <- function(x) attr(x, "labels", exact = TRUE)
var_label <- function(x) attr(x, "label", exact = TRUE)

# A labelled column of one of the three classes `:=` handles, built
# without haven or dtaparser: datakit's (with_labels()), haven's as
# haven::labelled() makes it (labels of the codes' type), and
# dtaparser's as dtaparser::with_labels() makes it (double labels).
CLASSES <- c("datakit_labelled", "haven_labelled", "dtaparser_labelled")

labelled_as <- function(class, x, labels, label = NULL) {
  switch(class,
    datakit_labelled = with_labels(x, labels, label = label),
    haven_labelled = structure(x, labels = haven_labels(x, labels), label = label,
                               class = c("haven_labelled", "vctrs_vctr", typeof(x))),
    dtaparser_labelled = structure(x, labels = stats::setNames(as.double(labels), names(labels)), label = label,
                                   class = c("dtaparser_labelled", "vctrs_vctr"))
  )
}

haven_labels <- function(x, labels) {
  whole <- !anyNA(labels) && all(labels == trunc(labels))
  if (is.integer(x) && whole) stats::setNames(as.integer(labels), names(labels)) else
    stats::setNames(as.double(labels), names(labels))
}
