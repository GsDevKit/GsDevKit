dispatching
handleWith: anObject

	(ProvideAnswerNotification signal: self prompt) ifNotNilDo: [:answer | ^answer ].
	^ anObject handleConfirmationRequest: self