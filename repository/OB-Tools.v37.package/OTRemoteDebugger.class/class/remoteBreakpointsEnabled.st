remote breakpoints
remoteBreakpointsEnabled

	EnableRemoteBreakpoints == nil ifTrue: [ EnableRemoteBreakpoints := true ].
	^EnableRemoteBreakpoints