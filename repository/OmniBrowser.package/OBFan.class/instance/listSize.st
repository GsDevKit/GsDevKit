accessing
listSize
	^ self children ifNil: [0] ifNotNil: [self children size]