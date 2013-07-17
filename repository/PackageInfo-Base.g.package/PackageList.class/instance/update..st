actions
update: aSymbol
	aSymbol = #packages ifTrue:
		[packages _ nil.
		self changed: #packageList; changed: #packageSelection]