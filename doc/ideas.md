# Cane

### Architecture
Cane has two running threads. One for sequencing a loop repeatedly in the background
and one for the user to write new expressions that will later be sequenced on the
other thread.

Cane will use a double buffering approach where the sequencer thread and compilation
thread have their own buffer that will be swapped when a new pattern is to be looped.

The user enters expressions that are compiled when they press enter. When the sequence
is compiled, it signals to the sequencer thread that it should stop looping the 
current sequence and instead swap the two buffers at the end of its current loop.


### User Interface
Cane only operates on a single MIDI channel/device. This is specified as a commandline
argument and cannot be changed at runtime. In cases where you want to use multiple
instruments, you will need to run multiple instances of cane.

Cane does not generate MIDI clock, it expects an incoming source from another device.
This means that in order to advance the sequence, you must send a MIDI start/stop message
in addition to clock pulses.

Some examples:
```
cane --channel 10 --device "TR-6S" --clock "Clocker"
cane --channel 14 --device "Casio CZ-1" --clock "TR-6S"
```


### Types
- `Scalar` scalars are integers used for things like note values
- `Melody` melodies are a collection of notes
- `Rhythm` rhythms are a collection of on/off events that can be mapped to a melody
- `Sequence` a sequence is a combination type of melody and rhythm
- `Pattern` a pattern is a collection of sequences

Cane will be strongly, statically typed with no special handling for type coercions.
The type system should be generally fairly straightforward with some special handling
for operator overloading on different types. For example, the behaviour for concatenating
sequences and patterns is different though they might look the same for the user.


### Operations
##### Scalar
- `+` / `-` scalar => scalar : prefix arithmetic operators (abs/neg)
- scalar `+` / `-` / `*` / `/` scalar => scalar : arithmetic operators
- scalar `lcm` / `gcd` scalar => scalar : lcm and gcd of two numbers. useful for polymeters
- scalar `:` scalar => rhythm : euclidean rhythm notation
- scalar scalar => melody : implicit concatenation of scalars to a melody
- scalar `=>` ident => scalar : assign scalar to identifier and evaluate to the expr
- `{` scalar+ `}` => scalar : pick a random scalar to evaluate to

##### Melody
- `'` melody => melody : reverses a collection of notes
- melody `@` rhythm => sequence : maps a rhythm to a collection of notes and turns it into a sequence
- melody `<` / `>` scalar => melody : shifts a melody left or right in a circularly way
- melody `+` / `-` / `*` / `/` scalar => melody : array-wise arithmetic operators that apply a scalar to all elements
- melody `**` scalar => melody : repeats a melody N times
- melody melody => melody : implicit concatenation of melodies
- melody `=>` ident => melody : assign melody to identifier and evaluate to the expr
- `{` melody+ `}` => melody : pick a random melody to evaluate to

##### Rhythm
- `~` rhythm => rhythm : inverts all of the active and inactive steps
- `'` rhythm => rhythm : reverses a rhythm
- rhythm `<` / `>` scalar => rhythm : shifts a rhythm left or right circularly
- rhythm `**` scalar => rhythm : repeats a rhythm N times
- rhythm rhythm => rhythm : implicitly concatenates rhythms
- rhythm `or` / `and` / `xor` rhythm => rhythm : bitwise operators that combine rhythms
- rhythm `=>` ident => rhythm : assign rhythm to identifier and evaluate to the expr
- `{` rhythm+ `}` => rhythm : pick a random rhythm to evaluate to

##### Sequence
- sequence sequence => sequence : implicitly concatenate sequences to the end of eachother
- sequence `=>` ident => sequence : assign sequence to identifier and evaluate to the expr
- sequence `/` / `*` scalar => sequence : divide an expressions time relative to the parent expressions time
- `{` sequence+ `}` => sequence : pick a random sequence to evaluate to

##### Pattern
- `[` sequence* `]` => pattern : layer a series of sequences together into a pattern so that we can play multiple notes concurrently (i.e. drum patterns or chords)
- pattern pattern => pattern : implicitly concatenates sequences pair-wise so that each pattern is sequenced one after the other 
- pattern `=>` ident => pattern : assign pattern to identifier and evaluate to the expr
- `{` pattern+ `}` => pattern : pick a random pattern to evaluate to


### Instructions
> The encoding passed to the sequencer used when generating MIDI events
