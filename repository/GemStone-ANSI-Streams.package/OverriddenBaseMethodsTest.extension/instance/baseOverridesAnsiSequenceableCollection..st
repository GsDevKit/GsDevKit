*gemstone-ansi-streams
baseOverridesAnsiSequenceableCollection: known

	self 
		addTo: known 
		forBehavior: SequenceableCollection 
		overrides: {
			#readStream -> 'readStream

"Returns a ReadStream on the receiver."

^ReadStream on: self
'.
			}