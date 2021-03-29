running
tearDown

	super tearDown.
	Breakpoint trappable: breakpointTrappable.
	(self class compiledMethodAt: #breakMethod) clearAllBreaks.
	(self class compiledMethodAt: #warningBreakMethod) clearAllBreaks.