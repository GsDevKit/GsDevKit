*ob-standard
doAutoCommit

	OBPlatform current doAutoCommit
		ifFalse: [OBInformRequest message: 'Auto commit failed. You will need to abort and try again.'].