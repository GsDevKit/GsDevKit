*squeak
squeakPrintString

	| aStream idx subStr lastIdx sz |
aStream := WriteStream on: String new.
aStream nextPut: $# .
idx := self indexOf: $' startingAt: 1 .
(idx == 0 _or:[ idx > 1000000 ]) ifTrue:[
  aStream nextPutAll: self .
  ]
ifFalse:[
  subStr := String new .
  lastIdx := 1.
  [ idx == 0 ] whileFalse: [
    self copyFrom: lastIdx to: idx into: subStr startingAt: 1 .
    aStream nextPutAll: subStr .
    subStr size: 0 .
    aStream nextPut: $' .
    lastIdx := idx + 1 .
    idx := self indexOf: $' startingAt: lastIdx .
    ].
  sz := self size .
  lastIdx <= sz ifTrue: [
    self copyFrom: lastIdx to: sz into: subStr startingAt: 1 .
    aStream nextPutAll: subStr .
    subStr size: 0 .
    ].
  ].
^aStream contents