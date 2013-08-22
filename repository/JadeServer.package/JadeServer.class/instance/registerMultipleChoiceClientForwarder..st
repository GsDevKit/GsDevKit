OmniBrowser
registerMultipleChoiceClientForwarder: aClientForwarder

	(OBGemStonePlatform respondsTo: #registerMultipleChoiceClientForwarder:)
		ifTrue: [ OBGemStonePlatform registerMultipleChoiceClientForwarder: aClientForwarder ]