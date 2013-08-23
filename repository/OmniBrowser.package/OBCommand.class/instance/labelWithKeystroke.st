accessing
labelWithKeystroke
	^ self keystroke
		ifNil: [self label]
		ifNotNil: [self label, ' (', self keystroke asString, ')']