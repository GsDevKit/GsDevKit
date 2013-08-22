*ob-tools
popTo: anOGStackFrame value: anObject

	self _trimStackToLevel: anOGStackFrame level.
	self _stepOverInFrame: 1 breakpointOnStack: false mode: false replace: true tos: anObject