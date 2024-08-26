## Introduction
A tiny word frequency counter.

## Requirement
GHC >= 9.4.8

## Build
Build up to an executable.
```bash
make
```

## Clean
Delete intermediate files.
```bash
make clean
```

## Example
Feed test file to program.
```bash
make test
```
This yields
```
word           times
--------------------
...
and            15
of             18
i              18
to             27
you            32
the            51
```
