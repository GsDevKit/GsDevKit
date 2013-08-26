private
requestVersionNameAndMessageWithSuggestion: nameString initialMessage: msgString
	^ (MCVersionNameAndMessageRequest new
		suggestedName: nameString;
		initialMessage: msgString
		) signal