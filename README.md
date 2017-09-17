IMP Compiler Implemented in Haskell
===================================

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

```zsh
% stack build
```

After this IMP programs can be compiled by running:

```zsh
% stack exec -- impc -o hello.ll examples/hello.imp
```

This generates LLVM assembly file. Use LLVM tools to further compile to
native assembler code or to produce an executable. An example using
[clang](https://clang.llvm.org):

```zsh
% clang -o hello hello.ll stdlib/impstd.c
% ./hello
Hello World!
```

NOTE: the standard library is currently incomplete.
