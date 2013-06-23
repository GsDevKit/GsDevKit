*squeak
sourceCodeAt: selector ifAbsent: aBlock

	| code |
	code := [self sourceCodeAt: selector] on: Error do: [:ex | ^aBlock value ].
	code == nil ifTrue: [ ^aBlock value ].
	^code