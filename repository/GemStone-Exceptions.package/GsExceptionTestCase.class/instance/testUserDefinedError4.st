tests
testUserDefinedError4

	| caughtIt text expected |
	caughtIt := false.
	text := 'testing'.
	expected := ExceptionA hasFixFor39741
		ifTrue: [ 'Halt: ', text ]
		ifFalse: [ text ].
	[self halt: text] 
		on: UserDefinedError 
		do: [:ex | 
			caughtIt := true.
			self assert: ex messageText = expected ].
	self assert: caughtIt.
