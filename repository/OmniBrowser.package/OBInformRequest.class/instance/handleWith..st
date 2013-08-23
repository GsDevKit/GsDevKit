dispatching
handleWith: anObject

	(ProvideAnswerNotification signal: self message) ifNotNilDo: [:answer | ^true].
	^ anObject handleInformRequest: self