- Update passes to take newly created type-specific node kinds
  - Maybe needs to be re-designed a bit because it adds a _tonne_ of cases.

- Implement function calls as postfix `()`

- Add handling for functions, layers, choice, patterns and sequences to parser/typechecker

- Evaluation pass that generates a list of events we can then serialise into
a MIDI file

- Refactor passes and parser to create melody/rhythm nodes
  - We don't need to print out or work with the actual values, the nodes
  simply need to exist so we can use them later in the evaluation passes

- Move all allocations to cane_alloc, cane_free etc.

- Actually free memory :v)

- Change operator kind in AST nodes after type checking
  - For example, scalars/melodies/rhythms share some operators but they are
  semantically different. We should probably have a SCALAR_ADD, MELODY_ADD kind
  - Logic already exists for doing this, just need to work out when to use it.

- LSHIFT/RSHIFT vs LROTATE/RROTATE
  - Scalars can be bit-shifted while melodies/rhythms can be rotated element-wise

- Graphviz
  - Make dot pass take a file pointer instead of file name
  - Wrap fopen/fclose and handle `cane_string_view_t` as filename

- Parsing for function type annotations

- Should assignment be made into an infix expression?
  - `is_binary` being an infix _or_ postfix operator doesn't really make sense
  for _true_ postfix operators

- Symbol remapping/binding power as part of symbol definitions

- cane_vector_t functions:
  - `at`            =>   indexing
  - `front`         =>   first element
  - `back`          =>   last element
  - `begin`         =>   pointer to first element
  - `end`           =>   pointer to last element
  - `is_empty`      =>   boolean check if length is 0
  - `length`        =>   return length (getter)
  - `capacity`      =>   return capacity (getter)
  - `cane_vec_info` =>   return buf pointer and length (cane_vector_info_t)
  - `fit`           =>   makes _at least_ enough space according to given size
  - `shrink`        =>   makes capacity equal to length (frees up memory)
  - `resize`        =>   sets size of vector (grow _or_ shrink) and sets cell to given value
  - `fill`          =>   sets all cells to given value
  - `clear`         =>   empties vector (keeps capacity)
  - `push`          =>   push elements to the end of the vector
  - `pop`           =>   pop elements from the end of the vector
  - `insert`        =>   insert at specific index and shift elements
  - `remove`        =>   remove element at specific index and shift elements
  - `concat`        =>   join two vectors together
  - `rotate`        =>   shift elements around circularly
  - `reverse`       =>   reverse elements in the vector
  - `repeat`        =>   concat the vector with itself N times
  - `sort`          =>   sort the elements of the vector
  - `swap`          =>   swap two indices in the vector
  - `broadcast`     =>   apply a given function to all elements of the vector with a given value
  - `eq`            =>   compare two vectors for equality
