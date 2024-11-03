# JSON To CFG

### Overview
- this program is a JSON parser in haskell, not using Parsec
- for purely educational purposes, it uses the context free grammar that represents JSON documents, and uses multiple stacks to deal with nondeterminisim
- effectively, it runs like a pushdown automaton as a result.

### Building and running
- to build, run cabal build. There are two executables:
  - **cabal run JSONToCFG** - this runs the main program, where you can input your own JSON strings
  - **cabal run Test** - this runs some testcases in /tests

 ### Known issues
 - it seems like we're hitting stack overflow on complex JSONs. This is to be updated soon, probably after exams.
