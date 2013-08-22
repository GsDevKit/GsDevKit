*ob-tools
completeStep: anOGStackFrame

	self _stepOverInFrame: anOGStackFrame level breakpointOnStack: anOGStackFrame breakpointOnStack  mode: false replace: false tos: nil.
	anOGStackFrame breakpointOnStack
		ifTrue: [
			self _stepOverInFrame: 1 breakpointOnStack: false mode: false replace: false tos: nil]