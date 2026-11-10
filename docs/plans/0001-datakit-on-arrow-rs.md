# Plan 0001: datakit on its own, with Parquet on arrow-rs

datakit comes out of the dta-parser repository so that it can be read
and reviewed on its own before it is used on real data. Three things
make it shorter there:

- Parquet is read and written by arrow-rs (the Apache Arrow project's
  Rust `parquet` crate) instead of about 12,000 lines of our own
  decoder and encoder.
- It no longer depends on dtaparser or datparser. You load either when
  you need it.
- polars support is left out. An issue here will track it, in case you
  want it later.

Labelled columns get haven's class, `haven_labelled`, built directly,
so haven does not need to be installed or loaded.

What stays the same: `d[i, name, by = g] := value`, `datakit_by()`,
`read_parquet()`, `read_parquet_info()` and `save_parquet()`, with the
arguments they have today except those listed under "Dropped", and
every file the old datakit wrote reads back the same.

## What the package will hold

About 3,100 lines in all, against about 23,800 lines of Parquet and
Arrow code in dta-parser plus datakit's 2,485 lines of R:

| part | lines (estimate) |
|---|---|
| Rust in `src/rust/`: open and read on arrow-rs, Arrow arrays to R vectors and back, the `dta` profile | ~2,140 |
| `src/init.c` (registration, the panic guard's C side) | ~80 |
| `R/parquet.R` | ~400 |
| `R/assign.R` (`:=`) | ~360 |
| `R/by.R` (`datakit_by()`) | ~90 |
| small files | ~30 |

The Rust parts, as estimated in the dta-parser plan:

| file | lines | what |
|---|---|---|
| `profile.rs` | ~500 | the `dta` entry in the file's key-value metadata, read with serde_json: labels, formats, value label sets, storage types, notes, characteristics, categoricals holding `.a` to `.z`; a warning per bad entry; a 64 MiB cap |
| `read.rs` | ~350 | open a file or raw vector, select columns, turn `skip`/`n_max` into a row selection, decode column groups on threads; with the file's offset index loaded, only the pages holding the rows are fetched (see "Row windows" below) |
| `to_r.rs` | ~450 | Arrow arrays to R vectors |
| `from_r.rs` | ~350 | R vectors to Arrow arrays; only `NA_real_`/`NA_integer_` become nulls, tagged NaNs (`.a` to `.z`) pass through bit for bit |
| `write.rs` | ~200 | writer properties, the profile, encoding columns in parallel |
| `info.rs` | ~150 | `read_parquet_info()` |
| `tagged.rs` | ~40 | the letter in a tagged NaN |
| `lib.rs` | ~100 | the `.Call` entries; a Rust panic becomes an R error, never a crash |

## Decisions to check before building

1. **`haven_labelled` without haven.** datakit builds the class
   (`c("haven_labelled", "vctrs_vctr", "double")`, with `labels` and
   `label` attributes) and gives `.a` to `.z` as haven's tagged NAs,
   which use the same bits as dtaparser's. haven's own functions
   (`na_tag()`, `as_factor()`, `zap_labels()`) work on the result once
   haven is loaded. The catch: the rules for combining two labelled
   columns (`bind_rows()`, `c()`, `if_else()`) live in haven. Without
   haven loaded, vctrs combines two `haven_labelled` columns only when
   all their attributes match: the same value labels, the same variable
   label and the same format. Any difference stops with an error.
   **Recommendation:** datakit defines no combining rules of its own,
   which would clash with haven's when both are loaded. Its `:=` combines
   labelled values itself, including the per-group results of `by =`,
   which today go through vctrs (see decision 2). The README says to
   load haven, or labelled, for `bind_rows()` and the like on columns
   whose attributes differ.
2. **Which labelled classes `:=` and `datakit_by()` accept.** dtatools
   passes dtaparser's `dtaparser_labelled` columns to `:=`, so `:=`
   must keep handling them without importing dtaparser.
   **Recommendation:** treat any column with a `labels` attribute alike:
   keep its class, change its data, and merge its `labels`. With `by =`,
   each group's result is joined without vctrs' combining rules: the
   values are put in place by row, and the groups' labels are merged
   with the same conflict checks as today (one text per code, letters
   matched by their tag). The tests run on both classes, and once with
   haven's namespace not loaded, on groups that return different labels,
   variable labels and formats.
3. **What `read_parquet()` gives for labels.** The old datakit gave
   `dtaparser_labelled`; the new one gives `haven_labelled`. The values,
   letters, labels, formats and variable labels stay the same; only the
   class name changes. Code that tests `inherits(x,
   "dtaparser_labelled")` on what `read_parquet()` returns must change.
4. **Text.** The old datakit returned dtaparser's packed strings, which
   R makes only when they are used. The new one makes ordinary R
   strings: one per distinct value for a dictionary-encoded column,
   otherwise one per row. A file of long text with many distinct values
   may read more slowly. PR 3 measures this.

## Dropped (your decisions in dta-parser)

- the polars functions (an issue here, for later)
- `decimal = "character"`: DECIMAL reads as a double, and a value with
  more than 15 significant digits is an error naming the column and row
- `nested = "skip"`: a nested column is still an error that names it
- reading brotli and Hadoop-LZ4 files
- refusing duplicate keys in the `dta` profile: serde_json reads it,
  and the last key wins
- the hand-made damaged-file and fuzz suites, replaced by a test that
  damaged fixtures give an R error, never a crash
- re-exports of dtaparser's and datparser's functions (`read_dta`,
  `save_dta`, `read_fixed_width`, the label helpers)

## Order of work

Each step is one pull request that stops ready for you to merge. They
stack, so each starts from the one before.

1. **This plan.**
2. **`:=` and `datakit_by()` without dtaparser.** Labelled columns are
   handled through their `label` and `labels` attributes (decision 2),
   with a small `na_tag` of the package's own. The tests port
   `test-assign.R`, `test-by.R` and `test-labelled.R`, run on both
   `haven_labelled` and `dtaparser_labelled` columns. CI also runs
   dtatools' tests against this datakit.
3. **The Parquet reader on arrow-rs.** `read_parquet()` and
   `read_parquet_info()`, with the tests ported from `test-parquet.R`,
   the compat fixtures against pyarrow's expected values, and the legacy
   fixtures dta-parser froze for this (`compat/parquet/legacy/`). It
   also measures Arrow-to-R conversion, warm and on the first call,
   against old datakit 3.1.0 installed in a scratch library. **Gate:**
   if whole-file reads take more than 1.25 times old datakit's warm
   time, the PR says so and stops for you to decide.
4. **The Parquet writer on arrow-rs.** `save_parquet()`, with every
   compression option; files the new writer makes must read
   `identical()` in old datakit; `readback.R` and `readback.py` check
   that arrow, nanoparquet, pyarrow, pandas, polars and DuckDB read
   them.
5. **Benchmarks and docs.** All 49 files of the dta-parser benchmark:
   warm and first-call times against old datakit 3.1.0, arrow and
   nanoparquet, reporting every case where the new code is slower. Also
   the README, `docs/parquet.md` (moved from dta-parser) and an ADR on
   arrow-rs.
6. **An issue on polars support**, saying what was removed and why.

## How the tests prove the same reads and writes

- **Differential.** A local script reads every legacy fixture, compat
  fixture and benchmark file with old datakit (scratch library) and new
  datakit, and compares values, `.a` to `.z` (by their bits), labels,
  variable labels, formats, factor levels and time zones. Only the
  labelled class may differ.
- **Frozen readings in CI.** The legacy fixtures carry what old datakit
  read from each (`<name>.read.rds`) and what pyarrow read (JSON). CI
  compares the new reader with both.
- **Other tools.** The readback scripts check that other readers read
  the files we write.

## Row windows

arrow-rs reads only the pages that hold the rows asked for when it has
the file's offset index, which it loads separately from the footer.
The reader asks for it. Files the old datakit, pyarrow (with
`write_page_index`) and polars write carry one. A file without one
still reads only the row groups that hold the rows, but within them
arrow-rs reads every page header to find the pages, so a small window
of a file with large row groups costs more. PR 3 tests this with a file
source that counts the bytes it is asked for: an indexed file must
fetch only the pages of the window.

## Not decided here

- Thread defaults: one rule replaces the old two (tibble and Arrow),
  set by measurement in PR 3 on the four files the dta-parser plan
  measured.
- The minimum Rust version is 1.88 (arrow-rs 60 needs it); CRAN-style
  offline builds vendor the crates as dtaparser does.
