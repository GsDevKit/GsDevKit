Error Handling
doesNotUnderstand: aMessageDescriptor

	caughtIt := true.
	aMessageDescriptor selector == #resendIssue159
		ifTrue: [  ^self perform: #nestedFooIssue159 ].
	aMessageDescriptor selector == #nestedFooIssue159
		ifTrue: [  ^super doesNotUnderstand: aMessageDescriptor ].
