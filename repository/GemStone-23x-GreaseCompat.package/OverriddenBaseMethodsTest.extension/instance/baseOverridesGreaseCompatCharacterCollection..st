*gemstone-23x-greasecompat
baseOverridesGreaseCompatCharacterCollection: known

	self 
		addTo: known 
		forBehavior: CharacterCollection 
		overrides: {
			#asInteger -> 'asInteger

"Returns an Integer whose value is represented by the receiver."

^ Integer fromString: self
'.
	}