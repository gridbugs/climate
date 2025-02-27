# Changelog

## Unreleased

### Changed

- Rename `desc` in APIs to `doc` to avoid confusion with the `description` field of manpages. (#12)

## 0.4.0

### Added

- Allow help messages colours to be configured (#7)
- Proof of concept of manpage generation (disabled in release as it's very incomplete) (#11)

### Changed

- Changed default help message colour scheme to be more colour-blind readable
  and more visible on light and dark terminals (#7)
- Changed description of `--help` argument.

### Fixes

- Remove superfluous style reset escape sequences (#7)
- Don't apply formatting to trailing spaces in argument names in help messages (#8)
- Print a readable error when the argument spec is invalid (#10)

## 0.3.0

### Added

- Color in help messages (#6)
- Allow documentation for positional args (#4)

### Changed

- Improvements to formatting of help messages (#5)
- Improve filename completion (#3)
- Stop printing spec errors (#2, fixes #1)

## 0.2.0

### Added

- Options for minifying generated completion scripts:
  - `--minify-global-names` rename all global functions and variables to use
    short generated names
  - `--minify-local-variables` rename local variables to single-letter names
  - `--no-comments` strip comments from the output
  - `--no-whitespace` remove all unnecessary whitespace including indentation
  - `--optimize-case-statements` detect sequences of cases with the same body
    and combine them

- `Arg_parser.Completion.stringify` removes the type constraint on a
  `Arg_parser.Completion.t` for situations where you want to use a completion
  for one type in a `conv` of a different type. Doing so is a little risky but
  there's no technical reason to forbid it and it may simplify writing
  parsers.

### Changed

- `Arg_parser.Completion.file` is now a `_ t` (formerly it was a `string t`).
  Sometimes you want to complete with files in a parser which doesn't yield
  paths, or use a non-string type to represent paths. E.g. `dune build` accepts
  build targets as arguments but it's convenient to be able to complete that
  command with files (at least until dune's completion script understands
  targets!).

- Completion scripts are indented by 2 spaces rather than 4 to make them more
  readable on small monitors.

- Refactored generated bash scripts to avoid generating a function for each
  argument. This is important as dune has many subcommands, each with many
  arguments, and generating a function for each one can create completion
  scripts over 1mb!

## 0.1.0

Initial release.
