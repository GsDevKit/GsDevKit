notification
handleMultiLineTextRequest: anOBTextRequest

	^(self clientForwarderMap at:#multiLineText) signalMultiLineTextRequestUsing: anOBTextRequest