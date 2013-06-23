*core-squeak
valueWithPossibleArgs: anArray

  | n arrSiz newArr |
  (n := self numArgs) == 0 ifTrue: [^ self value].
  n == (arrSiz := anArray size) ifTrue: [^ self valueWithArguments: anArray].
  newArr := Array new: n .
  anArray copyFrom: 1 to: (n > arrSiz ifTrue:[ arrSiz ] ifFalse:[ n ])
	    into: newArr startingAt: 1.  
  ^ self valueWithArguments: newArr