*core
baseOverridesCoreCharacterCollection: known

	self 
		addTo: known 
		forBehavior: CharacterCollection 
		overrides: {
			#match: -> 'match: prefix

"Returns true if the argument prefix is a prefix of the receiver, and
 false if not.  The comparison is case-sensitive."

self size == 0 ifTrue: [ ^ prefix size == 0 ].
^ self at: 1 equals: prefix
'.
	}