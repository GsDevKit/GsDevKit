accessing
collectionSpecies
	"Answer the species of collection into which the receiver can stream"
	
	^streamType == #binary
		ifTrue: [ ByteArray ]
		ifFalse: [ String ]