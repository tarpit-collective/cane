- add cmdline parsing for channels and bpm
- add channel lookup map in environment
- working function eval
- assignments and lookups
- layering operator
  - concat needs to fix up keys in rhs
  - layering doesnt need to fix up keys
- patterns support concat and layering
- mapping
- add token for newlines specifically so we can have them be valid break points for statements, can be skipped like any other token with `take`
- call discard_whitespace in every parser function

- add two melodies together
- head/tail operators

- print/debug operator to view sequences

- string_view of statement and other parent nodes should contain the extents of all those nodes. Basically the string view begin/end should include all expressions in the statement






------------------------------



- location at eof should be more clear (dont print a column number)



- how do we handle velocity? should we?
  - an accent can be applied to a sequence which is just an array of numbers like a melody

- sequences and patterns can be output to a midi channel by assigning it to a named channel
  - for example: `!...!... @ 65 . 64 ~> "drums"`
  - this can later be assigned an actual midi channel as a cli pair: `drums=0`

- Debugging with per-expression logging
  - We have access to location information in the AST so we can just print
  the source as we eval
  - For operator nodes, we can find print everything between the two symbols aswell

- Allow typing a unique subset of characters as part of a keyword for shorthand

- Cycles (as part of sequences) that allow alternating a sequence every bar
  - A way to generate a progression by just altering a subset of the sequence that
  rotates around every bar
  - Maybe "choice" should do this instead of randomness

- Evaluation pass that generates a list of events we can then serialise into
a MIDI file
