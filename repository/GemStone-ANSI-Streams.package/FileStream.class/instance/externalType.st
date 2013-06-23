accessing
externalType
	"Return a symbol that identifies the external stream type of the receiver."
	
	^self gsfile _isBinary
		ifTrue: [ #binary ]
		ifFalse: [ #text ]