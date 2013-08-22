callbacks
accept: aText notifying: aController
	| newSelector |
	newSelector := self compileMethod: aText notifying: aController.
	newSelector ifNil: [^ false].
	(self nodeFor: newSelector) demandSelection.
	OBPlatform current doAutoCommit
		ifFalse: [OBInformRequest message: 'Auto commit failed. You will need to abort and try to save again. The text in the window will be preserved across the abort boundary.'].
	^ true