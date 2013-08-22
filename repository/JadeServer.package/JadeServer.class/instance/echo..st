Testing
echo: anObject

	self addToPureExportSet: anObject.
	^anObject asOop printString.