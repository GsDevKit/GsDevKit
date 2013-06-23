*squeak
subclassTypeFor: aClass
	| type |
	type := aClass typeOfClass.
	type = #normal ifTrue: [^ ' subclass: '].
	type = #indexable ifTrue: [^ ' indexableSubclass: '].
	type = #variable ifTrue: [^ ' indexableSubclass: '].
	type = #bytes ifTrue: [^ ' byteSubclass: '].
	type = #transient ifTrue: [^ ' transientSubclass: '].
	self error: 'Unrecognized class type'