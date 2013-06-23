tests
testBug38799
	
	| gsException caughtIt resumed meth |
	meth := self class compiledMethodAt: #breakMethod.
	meth setBreakAtStepPoint: 1.	
	caughtIt := false.
	gsException := Exception 
		category: GemStoneError 
		number: 6005 
		do: [:ex :cat :num :args | 
			caughtIt := true.
			"needed to avoid infinite loop when resuming from a breakpoint"
			ex _incrementBreakpointsToIgnore. ].
	self breakMethod.
	self assert: caughtIt.
	gsException remove.
	gsException := Exception 
		category: GemStoneError 
		number: 6005 
		do: [:ex :cat :num :args |  
			BreakpointNotification signal.
			"needed to avoid infinite loop when resuming from a breakpoint"
			ex _incrementBreakpointsToIgnore. ].
	caughtIt := false.
	[self breakMethod] on: BreakpointNotification do: [:ex | caughtIt := true ].
	self assert: caughtIt.
	resumed := caughtIt := false.
	[self breakMethod.
	resumed := true] 
		on: BreakpointNotification 
		do: [:ex | 
			caughtIt := true.
			ex resume.
			"needed to avoid infinite loop when resuming from a breakpoint"
			ex _incrementBreakpointsToIgnore ].
	self assert: caughtIt.
	self assert: resumed.