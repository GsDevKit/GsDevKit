accessing
sender

	process stackDepth == nil ifTrue: [ ^nil ].
	index >= process stackDepth ifTrue: [ ^nil ].
	^self class previousStackFrame: self process: process index: (index + 1)