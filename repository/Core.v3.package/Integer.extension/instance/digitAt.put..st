*core-squeak
digitAt: n put: value 
    " Insert the value into the nth base 256 digit of the receiver. 
      GemStone natively uses base 4294967296 , do can't do a direct store."
  | nz wordIdx wordShift mask val word |
  value == 0 ifTrue: [ ^self ].
  (value < 0 or:[ value > 16rFF]) ifTrue:[ 
    OutOfRange signal: 'arg exceeds 16rFF unsigned'
  ].
  nz := n - 1 .
  wordIdx := (nz bitShift: -2 ) + 1 .
  wordShift := (nz bitAnd: 16r3) bitShift: 3  .
  mask := 16rFF bitShift: wordShift .
  val := (value bitAnd: 16rFF) bitShift: wordShift .
  word := self _digitAt: wordIdx .  
  word := (word bitAnd:  mask bitInvert ) bitOr: val .
  self _digitAt: wordIdx put: word .
^  self