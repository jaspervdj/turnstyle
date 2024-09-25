# ⊢ Turnstyle

Turnstyle is a graphical esoteric programming language.  For more information on
the language, see <http://jaspervdj.be/turnstyle>.

This repository holds:

 -  The reference implementation in Haskell
 -  A JavaScript implementation that works in the browser
 -  The turnstyle website
 -  An experimental compiler

## Local development

You need a Haskell installation to build this repository.  The recommended way
to do that is to use [GHCup](https://www.haskell.org/ghcup/).

 -  Running a program: `cabal run turnstyle run FOO.PNG`
 -  Run the website: `cabal run turnstyle-website preview`
 -  Running the tests: `cabal test`
