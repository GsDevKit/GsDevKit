tests
testZeroDivide

	| caughtIt |
	caughtIt := false.
	[ ZeroDivide dividend: 7 ] 
		on: Error
		do: [:ex | caughtIt := ex gsNumber == ZeroDivide errorNumberToSignal ].
	self assert: caughtIt