# Type Interactions
## Primitives
- Scalar
- Melody
- Rhythm
- Sequence
- Pattern

## Operations

## List of Type Mappings
`+` scalar -> scalar
`-` scalar -> scalar
`&` scalar -> melody

`'` melody -> melody

`~` rhythm -> rhythm
`'` rhythm -> rhythm

scalar `+` scalar -> scalar
scalar `-` scalar -> scalar
scalar `*` scalar -> scalar
scalar `/` scalar -> scalar
scalar `<<` scalar -> scalar
scalar `>>` scalar -> scalar
scalar `lcm` scalar -> scalar
scalar `gcd` scalar -> scalar
scalar `:` scalar -> rhythm
scalar `.` scalar -> melody

melody '@' rhythm -> sequence
melody '<' scalar -> melody
melody '>' scalar -> melody
melody `+` scalar -> melody
melody `-` scalar -> melody
melody `*` scalar -> melody
melody `/` scalar -> melody
melody `**` scalar -> melody
melody `.` melody -> melody

rhythm '@' melody -> sequence
rhythm `<` scalar -> rhythm
rhythm `>` scalar -> rhythm
rhythm `**` scalar -> rhythm
rhythm `.` rhythm -> rhythm
rhythm `or` rhythm -> rhythm
rhythm `xor` rhythm -> rhythm
rhythm `and` rhythm -> rhythm

sequence `.` sequence -> sequence
sequence `*` scalar -> sequence
sequence `/` scalar -> sequence

scalar `=>` ident -> scalar
melody `=>` ident -> melody
rhythm `=>` ident -> rhythm
sequence `=>` ident -> sequence








