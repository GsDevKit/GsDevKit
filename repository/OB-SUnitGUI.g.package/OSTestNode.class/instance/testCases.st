accessing
testCases

	root result == nil ifTrue: [ ^#() ].
	^root result hasPassed
		ifTrue: [ #() ]
		ifFalse: [ | testCases |
			testCases := OrderedCollection new.
			root result errors do: [:ea | 
				testCases add: ((OSTestCaseNode onError: ea) root: root; yourself) ].
			root result failures do: [:ea | 
				testCases add: ((OSTestCaseNode onFailure: ea) root: root; yourself) ].
			testCases]