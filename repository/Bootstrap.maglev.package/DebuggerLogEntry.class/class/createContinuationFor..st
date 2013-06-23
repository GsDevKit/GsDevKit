continuation creation
createContinuationFor: aDebuggerLogEntry
	"snap off a continuation at this point and stash it in the queue"
	
	|action continuation |
	action := [:cont | continuation := cont. #create ] callCC.
	action == #create
		ifTrue: [ (aDebuggerLogEntry continuation: continuation) addToLog ].
	action == #debug
		ifTrue: [ | meth |
			meth := self class compiledMethodAt: #remoteBreakpointMethod.
			meth setBreakAtStepPoint: 1.
			self remoteBreakpointMethod ].
