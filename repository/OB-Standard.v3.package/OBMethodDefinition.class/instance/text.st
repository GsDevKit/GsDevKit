callbacks
text
	^ source 
		ifNotNil: [source]
		ifNil: [self theClass sourceCodeTemplate]