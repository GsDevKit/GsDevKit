tests
testHaltWithMessage

	| caughtIt text expected |
	caughtIt := false.
	text := 'message'.
	expected := ExceptionA hasFixFor39741
		ifTrue: [ 'Halt: ', text ]
		ifFalse: [ text ].
	[
		self halt: text.
	] on: Halt do: [:ex | 
		caughtIt := true.
		self assert: ex messageText = expected.
	].
	self assert: caughtIt.
