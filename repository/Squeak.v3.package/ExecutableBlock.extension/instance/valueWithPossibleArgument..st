*squeak
valueWithPossibleArgument: anArg 
     "Evaluate the block represented by the receiver. 
     If the block requires one argument, use anArg, if it requires more than one,
     fill up the rest with nils."
  | n arr | 
  (n := self numArgs) == 0 ifTrue: [^self value].
  n == 1 ifTrue: [^self value: anArg].
  n == 2 ifTrue: [ ^ self value: anArg value: nil ].
  n == 3 ifTrue:[ ^ self value: anArg value: nil value: nil ].
  (arr := Array new: n) at: 1 put: anArg .
  ^ self valueWithArguments: arr .