other
gsArguments

	^(gsArguments notEmpty)
		ifTrue: [gsArguments]
		ifFalse: [ | msg |
			msg := self messageText.
			msg = '#halt encountered'
				ifFalse: [ msg := 'Halt: ', msg ].
			Array with: #'halt' with: msg ].
