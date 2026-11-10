# AGENTS.md

datakit is an R package for data preparation: `d[i, name, by = g] :=
value`, `datakit_by()`, labelled columns that keep `.a` to `.z`, and
(from plan 0001's later steps) Parquet files read and written by
arrow-rs. It came out of the dta-parser repository
(github.com/jbearak/dta-parser), where its history and decisions before
2026-11-10 are. Plans live in `docs/plans/`, decisions in `docs/adr/`.

## Rules that override everything else

- Never run Stata. It is not installed on this machine; do not install it
  or look for another copy.
- Never read Stata's installed files: ado files, help files, or anything
  else that ships with Stata. Do not use tools or data extracted from them.
- Never use output Stata produced as a test oracle or expected result.
  Expected values come only from open-source libraries (haven, ReadStat,
  pandas, readstata13, pyarrow and the like) and from the tests' own
  assertions.
- Make fixtures without Stata: write them with this package or
  dta-parser's writers, or with haven, pandas, pyarrow or ReadStat, from
  data you create. Do not use example datasets that ship with Stata.
- Take facts about the `.dta` format and Stata's rules only from general
  knowledge and from open-source projects' documentation and code. Do not
  look up or use official Stata documentation, and never copy text from
  it; write in your own words. Copy open-source code only if its license
  allows, with credit.
- If you think you have found a bug in another project (haven, vctrs,
  arrow-rs, pyarrow or any other), read dta-parser's
  `docs/other-readers.md` first: the differences found so far were either
  fixed upstream already or defensible choices. Never file an issue, pull
  request or comment in another project without the user's approval of
  that specific report.
- Work only in this repository. Other repositories, dta-parser among
  them, may be read, and never pushed to.

## Publishing

Ask before publishing anything to CRAN, r-universe or any other
registry.
