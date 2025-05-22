# Mission statement

Much like the many other _language-X_ Haskell libraries on Hackage and
elsewhere, `language-haskell` provides the basic implementation of the Haskell
programming langugage. Quite commonly for this crowd, this currently includes
a parser and pretty printer and not much else.

*Which* Haskell you might ask?

The answer is all of them, aspirationally at least. The library covers the
Haskell 2010 standard thoroughly, but it also supports a large number of GHC
extensions. One goal of the library is to cover all known language extensions,
while keeping any of them from compromising either the standard language base
or the other extensions.

# Design

To make this goal possible, the library relies on the finally-tagless
encoding of the AST. It also keeps the language grammar modular: every extension
that affects the language grammar defines an optional grammar overlay. The grammar
used for parsing a particular Haskell module is composed for the purpose, based
on the list of extensions it declares. The `grammatical-parsers` library provides
all the tools used for this.

Every parsed AST node is contained by a wrapper node. Immediately after parsing,
the wrapper carries the information about the node's position and the tokens
consumed to parse it. This information is sufficient to xactly reproduce the
original source code. In the next stage, the identifiers are bound to their
definitions and the wrappers get enriched to carry the resolved environment. The
discovered bindings are subsequently used to verify and correct the AST of
sequences of infix and prefix operators.

After the AST goes through some transformations, it can be converted to Template
Haskell expressions and finally pretty-printed.
