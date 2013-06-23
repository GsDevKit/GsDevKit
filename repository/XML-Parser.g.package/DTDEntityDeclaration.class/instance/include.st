behaviors
include
	"Return my expanded value."
	^value ifNil: [SAXWarning signal: 'XML undefined entity ' , name printString]