# Exercism wizard (WIP)

A wrapper around [exercism cli](https://github.com/exercism/cli) to provide unified interface to common operations.

Note that most of the stuff are just customized for myself, open to suggestions though.

## Features

Put built binary (which should be called `ew`) somewhere in `$PATH`, then:

- `ew proxy <args...>`: proxy mode, same as executing `exercism <args...>`.

## Planned features

Planned features by priority:

- `ew test`: do whatever command necessary to run tests
- `ew lint`: run language specific linter
- `ew rmignore`: remove "ignore" annotation from tests
- `ew fmt`: format source code
- `ew get`: fetch problem template
- `ew on`: spawn a sub-shell and switch to problem's project directory (might download problem template if missing
- `ew submit`: submit default stuff, or alternatively an explicit list can be given.
- `ew peekrepo`: open a URL to language repo - for taking a look at problems locked away
- `ew peeksol`: open a URL to community solutions