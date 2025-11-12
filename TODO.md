- add two melodies together
- head/tail operators

- Make coerce prefix
- Use same design language as lexer for the parser with monads
- string_view of statement and other parent nodes should contain the extents of all those nodes. Basically the string view begin/end should include all expressions in the statement
- Generate sequence events
- Better type errors and type handling in general
- Generate is_prefix/is_primary/is_postfix from symbol remappings






------------------------------



- location at eof should be more clear (dont print a column number)


- rethink binding power definition, use old approach from cane where we have LAST & INCR


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

- Update passes to take newly created type-specific node kinds
  - Maybe needs to be re-designed a bit because it adds a _tonne_ of cases.

- Add handling for functions, layers, choice, patterns and sequences to parser/typechecker

- Evaluation pass that generates a list of events we can then serialise into
a MIDI file

- Change operator kind in AST nodes after type checking
  - For example, scalars/melodies/rhythms share some operators but they are
  semantically different. We should probably have a SCALAR_ADD, MELODY_ADD kind
  - Logic already exists for doing this, just need to work out when to use it.

- LSHIFT/RSHIFT vs LROTATE/RROTATE
  - Scalars can be bit-shifted while melodies/rhythms can be rotated element-wise

- Should assignment be made into an infix expression?
  - `is_binary` being an infix _or_ postfix operator doesn't really make sense
  for _true_ postfix operators

- Symbol remapping/binding power as part of symbol definitions
