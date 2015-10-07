instance creation
previousStackFrame: anOGStackFrame process: aProcess index: anInteger

	| new |
	new := self new.
	new previousStackFrame: anOGStackFrame process: aProcess index: anInteger.
	^new