*squeak
kindOfSubclass
	| type |
	type := self typeOfClass.
	type = #normal ifTrue: [^ ' subclass: '].
	type = #transient ifTrue: [^ ' transientSubclass: '].
	type = #indexable ifTrue: [^ ' variableSubclass: '].
	type = #variable ifTrue: [^ ' variableSubclass: '].
	type = #bytes ifTrue: [^ ' variableByteSubclass: '].
	self error: 'Unrecognized class type'