running
setUp
	super setUp.
	GsProcess usingNativeCode
		ifTrue: [ 
			| p |
			p := GsProcess _current.
			[ p convertToPortableStack ] fork.
			Processor yield ].
	GsProcess _current breakpointLevel: self _breakpointLevel.
	(self class compiledMethodAt: #'breakMethod') clearAllBreaks.
	(self class compiledMethodAt: #'warningBreakMethod') clearAllBreaks