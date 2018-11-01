IMP Compiler Implemented in Haskell
===================================

[![Build Status](https://travis-ci.org/eugmes/imp.svg?branch=master)](https://travis-ci.org/eugmes/imp)

This is an implementation of IMP compiler in Haskell programming
language. It uses [LLVM](http://llvm.org) as compiler backend.
[Megaparsec](https://hackage.haskell.org/package/megaparsec) is used
for parsing.

The programming language is specified in this document
<http://gcmuganda.faculty.noctrl.edu/classes/Spring12/306/Imp.pdf>.

This work is inspired by [Hakell LLVM Tutorial](http://www.stephendiehl.com/llvm/)
by Stephen Diehl.

Building
--------

It is recommended to use [stack](https://docs.haskellstack.org/en/stable/README/)
to build the program:

```
% stack build
```

After this IMP programs can be compiled by running:

```
% stack exec -- impc examples/hello.imp
% ./hello
Hello World!
```

This uses [clang](https://clang.llvm.org). To specify a different compiler,
use `--cc` command line option or `CC` environment variable.

Extensions
----------

The language has several extensions compared to the original specification.
All of the extensions are taken from [Ada](http://www.ada-auth.org/standards/12rm/html/RM-TOC.html):

  - String literals can have embedded quotation marks by repeating them twice.

  - If expressions can have additional `elsif` parts.
