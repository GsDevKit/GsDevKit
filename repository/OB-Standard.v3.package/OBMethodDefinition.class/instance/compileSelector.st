accessing
compileSelector
	^self compileText ifNotNil: [self getSelector: self compileText]