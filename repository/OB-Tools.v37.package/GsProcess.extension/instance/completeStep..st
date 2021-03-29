*ob-tools
completeStep: anOGStackFrame
	self
		_stepOverInFrame: anOGStackFrame level
		mode: false
		replace: false
		tos: nil
		breakpointLevel: 0