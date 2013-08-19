asserting
assertScans: anArray
	self assert: anArray equals: (MCScanner scan: anArray squeakPrintString readStream)