running
tearDown
	super tearDown.
	(self class compiledMethodAt: #'breakMethod') clearAllBreaks.
	(self class compiledMethodAt: #'warningBreakMethod') clearAllBreaks