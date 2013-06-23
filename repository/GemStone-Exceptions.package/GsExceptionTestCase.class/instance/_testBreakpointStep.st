tests
_testBreakpointStep
	"To test interactively: when debugger comes up, go to #complexBreakMethod and press the step method ..."
	
	| caughtIt meth |
	meth := self class compiledMethodAt: #complexBreakMethod.
	meth setBreakAtStepPoint: 8.	
	caughtIt := false.
	Exception category: GemStoneError number: 6005 do: [:ex :cat :num :args | 
		ex remove.
		BreakpointNotification signal].
	[self complexBreakMethod] on: BreakpointNotification do: [:ex | caughtIt := true. self halt ].
	self assert: caughtIt
