running
setUp

	super setUp.
	GsProcess usingNativeCode ifTrue:[ | p |
		p := GsProcess _current .
		[ p convertToPortableStack ] fork .
		Processor yield .
	].
	breakpointTrappable := Breakpoint trappable: true.
	(self class compiledMethodAt: #breakMethod) clearAllBreaks.
	(self class compiledMethodAt: #warningBreakMethod) clearAllBreaks.