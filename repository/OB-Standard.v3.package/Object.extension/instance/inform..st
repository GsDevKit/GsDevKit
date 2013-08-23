*ob-standard
inform: aString
	"Display aString to the user."

	OBGemStonePlatform canInform ifFalse: [ ^self ].
	OBInformRequest message: aString