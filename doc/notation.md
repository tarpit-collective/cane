// SCALAR
scalar_prefix =
scalar_infix =
scalar_postfix =

scalar = int | '(' scalar ')'


// RHYTHM
steps = ( '!' | '.' | '?' )+
tuplet = ( '/' | '*' ) scalar rhythm

rhythm_prefix =
  ( 'head' | 'tail' | 'rev' | 'inv' | 'len' ) rhythm

rhythm_infix = 
  rhythm ( '|' | ',' | 'or' | 'and' | 'count' ) rhythm 

rhythm_postfix =
  rhythm ( '<<' | '>>' | '**' | 'size' ) scalar |
  rhythm '=>' ident

rhythm_ops = rhythm_prefix | rhythm_infix | rhythm_postfix

euclidean = 'euc' scalar scalar

rhythm =
  steps |
  tuplet |
  rhythm_ops |
  '(' rhythm ')'


// MELODY
array = '[' expr* ']'



  