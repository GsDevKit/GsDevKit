tests
testIssue159

	| caught |
	caught := false.
	[ 1 foo ] on: MessageNotUnderstood do: [:ex | caught := true ]. 
	self assert: caught.
	caught := false.
	[ 1 perform: #foo ] on: MessageNotUnderstood do: [:ex | caught := true ]. 
	self assert: caught.
