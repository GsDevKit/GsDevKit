OmniBrowser
openDebuggerOn: aProcess

	| debuggerClass |
	debuggerClass := OTDebugger.
	(aProcess localMethodAt: 1) selector == #remoteBreakpointMethod
		ifTrue: [Smalltalk at: #OTRemoteDebugger ifPresent: [:cl | debuggerClass := cl ]].
	^debuggerClass process: aProcess