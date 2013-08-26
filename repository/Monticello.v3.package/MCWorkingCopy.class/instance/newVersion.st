operations
newVersion
	^ (self requestVersionNameAndMessageWithSuggestion: self uniqueVersionName
		initialMessage: self patchMessageSuggestion) ifNotNil:
			[:pair |
			self newVersionWithName: pair first withBlanksTrimmed
				message: (self patchMessageStripped: pair last)].
