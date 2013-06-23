*bootstrap
testRunTestWithResource
	| x |
	self clearOuterResourceStateDuring:
		[self assert: (x := (self class selector: #dummy) run printString)
					= '1 run, 1 passes, 0 expected defects, 0 failures, 0 errors, 0 unexpected passes'
			description: 'A dummy test that needed a resource did not pass'].
