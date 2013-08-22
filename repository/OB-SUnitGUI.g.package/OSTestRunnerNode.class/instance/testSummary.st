accessing
testSummary

	^result == nil
		ifTrue: [ '0 run, 0 passes, 0 expected failures, 0 failures, 0 errors, 0 unexpected passes' ]
		ifFalse: [ result printString ]