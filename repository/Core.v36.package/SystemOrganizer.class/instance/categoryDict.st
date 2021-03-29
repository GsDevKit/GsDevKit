private
categoryDict

	categoryDict == nil ifTrue: [ categoryDict := ClassOrganizer cachedOrganizer categories ].
	^categoryDict