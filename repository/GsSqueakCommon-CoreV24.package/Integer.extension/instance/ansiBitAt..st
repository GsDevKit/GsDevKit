*gssqueakcommon-corev24
ansiBitAt: argInt
  "Returns the bit at the ith position of the receiver, where 1 is the least
 significant bit. (see https://github.com/GsDevKit/GsDevKit/issues/49)"

  | i temp digitIndex index |
  argInt _validateClass: SmallInteger.
  argInt < 1
    ifTrue: [ ^ i _error: #'rtErrArgNotPositive' ].
  i := argInt - 1.	"i is zero-based"
  digitIndex := i // 15 + 1.	"15 bits per digit, get a small integer"
  temp := self _digitAt: digitIndex.
  self negative
    ifTrue: [ 
      temp := 32767 - temp.
      index := 1.
      [ index < digitIndex & ((self _digitAt: index) == 0) ]
        whileTrue: [ index := index + 1 ].
      index == digitIndex
        ifTrue: [ temp := temp + 1 ] ].
  (temp bitAnd: (1 bitShift: i \\ 15)) == 0
    ifTrue: [ ^ 0 ]
    ifFalse: [ ^ 1 ]