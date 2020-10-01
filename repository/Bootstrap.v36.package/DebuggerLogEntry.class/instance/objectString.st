displaying
objectString

	^self hasContinuation
		ifTrue: [ super labelString ]
		ifFalse: [ super objectString]