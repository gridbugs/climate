# Climate

[![test status](https://github.com/gridbugs/climate/actions/workflows/test.yml/badge.svg)](https://github.com/gridbugs/climate/actions/workflows/test.yml)

A boring command-line parser for OCaml.

## Manual

### Terminology

__Term__ will refer to each space-delimited string on the command line after the
program name. The command `ls -l --color=always /etc/` has 3 terms. The program
name is `ls` (not a term), and the terms are `-l`, `--color=always`, and
`/etc/`.

__Argument__ will refer to each distinct piece of information passed to the
program on the command line. The command `make -td --jobs 4 all` has 4
arguments. The `-td` term is made up of two arguments combined into a single
term: `-t` and `-d` (more on this later). `--jobs 4` is a single argument
comprising two terms, where `4` is a parameter to the argument `--jobs`. The
final term `all` is also an argument.

Arguments may be __positional__ or __named__. Positional arguments are
identified by their position in the argument list rather than by name. Named
arguments may have two forms: __short__ and __long__. Short named arguments
begin with a single `-` followed by a single non `-` character, such as `-l`.
Long named arguments begin with `--` followed by one or more non `-` characters,
such as `--jobs`. A collection of short named arguments may be combined together
with a single leading `-` followed by each short argument name. For example in
`ls -la`, the `-la` is an alternative way of writing `-l -a`.

A named argument may take a __parameter__. A parameter is a single value which
follows the argument on the command line. Using `make`'s `--jobs` argument as an
example, here are the different ways of passing a parameter to a named argument
on the command line:

```
make --jobs=4   # long name with equals sign
make --jobs 4   # long name space delimited
make -j 4       # short name space delimited
make -j4        # short name without space
```

If multiple short arguments are combined into a single term then only one of
those arguments may take a parameter. If the parameterized argument appears as
the final argument in the sequence then the following term will be treated as
its parameter, such as in `make -dj 4`, which is equivalent to `make -d -j 4`.
If the parameterized argument appears in a non-final position within the
sequence then the remainder of the sequence is treated as its parameter, such as
in `make -dj4` which is also equivalent to `make -d -j 4`.
