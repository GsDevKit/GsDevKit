*core-squeak
indexOfAnyOf: aByteArray startingAt: start ifAbsent: aBlock
  "returns the index of the first character in the given set, starting from start"

  | ans |
  ans := self class
    findFirstInString: self
    inSet: aByteArray asByteArray byteArrayMap
    startingAt: start.
  ans = 0
    ifTrue: [ ^ aBlock value ]
    ifFalse: [ ^ ans ]