private
addResolved: aResolvedReference
	| version reference |
	version := aResolvedReference version.
	(self versions includes: version)
		ifTrue: [ ^ self ].
	self versions addLast: version.
	version dependencies do: [ :dependency |
		self addResolved: (GoferResolvedReference
			name: dependency versionInfo name
			repository: aResolvedReference repository) ]