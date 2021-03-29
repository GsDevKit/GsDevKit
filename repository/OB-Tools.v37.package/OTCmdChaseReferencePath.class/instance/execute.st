execution
execute
	| refs |
	OBPlatform current doAutoCommit
		ifFalse: [OBInformRequest message: 'Auto commit failed. You will need to abort and try again.'].
	refs := SystemRepository findReferencePathToObject: target value.
	refs explore