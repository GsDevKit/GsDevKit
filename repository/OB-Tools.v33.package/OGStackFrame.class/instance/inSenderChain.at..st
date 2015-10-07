testing
inSenderChain: anOGStackFrame at: aStepPoint

	| s |
	self = anOGStackFrame 
		ifTrue: [ ^true ]
		ifFalse: [
			self homeContext = anOGStackFrame
				ifTrue: [
					aStepPoint = stepPoint 
						ifTrue: [ 
							"target step point is on the stack, so we're done"
							^false ]]].
	(s := self sender) ~~ nil ifTrue: [ ^s inSenderChain: anOGStackFrame at: aStepPoint ].
	"no longer in sender chain - returned from method"
	^false