*ob-standard
confirm: aString

	OBGemStonePlatform canConfirm ifFalse: [ ^true ].
	^OBConfirmationRequest
		prompt: aString