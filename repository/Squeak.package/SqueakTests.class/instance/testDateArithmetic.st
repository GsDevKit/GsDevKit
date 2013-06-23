testing
testDateArithmetic
	| x y|
	self assert: (Date today + 1 day) = Date tomorrow.
	self assert: (Date today - 1 day) = Date yesterday.
	self assert: (Date today + 12 hours) = Date today.
	self assert: (Date today - 12 hours) = Date yesterday.
	self assert: (Date tomorrow - Date today) days = 1.
	x := Time now asDateAndTime.
	y := DateAndTime now + 1 seconds.
	Transcript cr; show: 'x: ', x printString, ' y: ', y printString.
	self assert: x  <= y
