ANSI support
handles: anExceptionSelector

	^anExceptionSelector == GemStoneError or: [
		super handles: anExceptionSelector.
	].
