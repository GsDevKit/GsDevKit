*ob-tools
popTo: anOGStackFrame value: anObject
	self _trimStackToLevel: anOGStackFrame level.
	self
		_stepOverInFrame: anOGStackFrame level
		mode: false
		replace: false
		tos: nil
		breakpointLevel: 0