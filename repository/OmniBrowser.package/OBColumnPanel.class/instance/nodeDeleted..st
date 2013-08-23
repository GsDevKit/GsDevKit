updating
nodeDeleted: ann
	ann node = self root ifTrue: 
		[current _ nil.
		self columns first clear.
		self announcer announce: (OBSelectionChanged column: self)].