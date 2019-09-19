# Source code

This is the source code for several prototypes of pattern-match
compilation algorithms extended with pattern guards.

## Roadmap

* base -- definition of the language that serves as both the source
  and target of pattern-match compilation

* common -- common utility code used by the various compilers

* classic-bt -- The *Classic* Backtracking algorithm from

* optimized-bt -- The *Optimized* Backtracking algorithm from
  *Optimizing Pattern Matching* by Fabrice Le Fessant and
  Luc Maranget (ICFP 2001).

* pettersson-dt -- The Compilation to DFA algorithm from
  Pettersson's Ph.D. dissertation.

* examples -- Various examples of match cases

