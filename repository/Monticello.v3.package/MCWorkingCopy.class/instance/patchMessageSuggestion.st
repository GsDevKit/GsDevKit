operations
patchMessageSuggestion
	^	self patchMessageDefault, String cr, String cr,
		self patchMessageChangesDelimiter, String cr,
		self patchMessageChangesHeader, String cr,
		self patchMessageChanges