as yet unclassified
filter: aFilter
	filter _ aFilter.
	filter ifNotNil:
		[filter activate.
		self changed: #list]