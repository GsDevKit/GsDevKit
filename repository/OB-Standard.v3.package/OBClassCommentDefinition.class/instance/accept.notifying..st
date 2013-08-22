callbacks
accept: aText notifying: aController

	| res |
	res := super accept: aText notifying: aController.
	self signalChange.
	OBPlatform current doAutoCommit
		ifFalse: [OBInformRequest message: 'Auto commit failed. You will need to abort and try to save again. The text in the window will be preserved across the abort boundary.'].
	^res