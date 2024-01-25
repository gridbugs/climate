# Climate

A boring command-line parser for OCaml.

## Manual

### Basics

Command-line arguments may be named or positional. Named arguments are
identified by a single letter "short name" or a multi-letter "long name". Named
arguments may take a parameter. Whether or not an argument takes a value is
_statically known_. That is, there are no arguments which optionally take
parameters.

#### Examples

`foo` is a positional argument at position 0. `bar` is a positional argument at
position 1.
```
prog foo bar
```
---

The interpretation of this command depends on whether the argument `--foo` is
specified to take a parameter. If `--foo` takes a parameter then `bar` is
interpreted as the parameter to `--foo`, but if `--foo` doesn't take a parameter
then `bar` is treated as a positional argument (at position 0).
```
prog --foo bar
```
