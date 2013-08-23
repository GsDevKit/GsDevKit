morphic
labelWithKeystroke
	^keystroke
		ifNil: [label]
		ifNotNil: [label, ' (', keystroke asString, ')']