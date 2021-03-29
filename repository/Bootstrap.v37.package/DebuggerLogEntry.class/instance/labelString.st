displaying
labelString

	^self hasContinuation
		ifTrue: [ self continuationLabel ]
		ifFalse: [ super labelString ]