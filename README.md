# Mission statement

Much like the many other _language-X_ Haskell libraries on Hackage and
elsewhere, `language-haskell` provides the basic implementation of the Haskell
programming langugage. Quite commonly for this crowd, this currently includes
a parser and pretty printer and not much else.

*Which* Haskell you might ask?

The answer is all of them, aspirationally at least. The library covers the
Haskell 2010 standard thoroughly, but it also support a large number of GHC
extensions. One goal of the library is to cover all known language extensions,
while keeping any of them from compromising either the standard language base
or the other extensions.

To make this goal possible, the library relies on the finally-tagless
encoding. It also keeps the language grammar modular: every extension that
affects the language grammar defines an optional grammar mixin. The grammar
used for parsing a particular Haskell module is composed for the purpose based
on the list of declared extensions.

At the moment, there are two finally-tagless type classes and two sets of AST
types that instantiate them: the standard Haskell 2010, and the fully-extended
GHC Haskell as far as it got. The plan is to eventually split up the
extensions type class into many classes, one per extension.
