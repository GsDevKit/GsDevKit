*core-squeak
digitAt: n
    "Answer the nth base 256 digit in the receiver."
 | abs nz wordIdx digit res |
 abs := self < 0 ifTrue:[ 0 - self ] ifFalse:[ self ].
 nz := n - 1 .
 wordIdx := (nz bitShift: -2 ) + 1 .
 digit := abs _digitAt: wordIdx  .
 res := (digit bitShift: (nz bitAnd: 16r3) * -8 ) bitAnd: 16rFF .
 ^ res