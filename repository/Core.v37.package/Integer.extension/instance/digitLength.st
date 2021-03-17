*core-squeak
digitLength
    "Returns the number of base 256 digits in the receiver, without counting
     any leading zeros."
  | n abs wordShift base word |
  abs := self < 0 ifTrue:[ 0 - self ] ifFalse:[ self ].
  abs _isSmallInteger ifTrue:[ 
    base := 0 .
    word := abs .
  ] ifFalse:[ | len |
    len := abs _digitLength .
    word := abs _digitAt: len  .
    base := (len - 1) * 4  .
  ].
  wordShift := 8 .
  n := 1 .
  [ true  ] whileTrue:[
    word < (1 bitShift: wordShift ) ifTrue:[ ^ n + base ].
    wordShift := wordShift + 8  .
    n := n + 1 .
  ].