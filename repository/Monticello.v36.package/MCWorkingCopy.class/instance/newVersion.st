operations
newVersion
	^ (self requestVersionNameAndMessageWithSuggestion: self uniqueVersionName) ifNotNilDo:
		[:pair |
		self newVersionWithName: pair first message: pair last].