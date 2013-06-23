other
gsArguments

	^(gsArguments notEmpty)
		ifTrue: [gsArguments]
		ifFalse: [Array with: #'genericError' with: messageText with: #()].
