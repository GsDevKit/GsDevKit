accessing
targetContext

	targetContext == nil ifTrue: [ ^process suspendedContext ].
	^targetContext