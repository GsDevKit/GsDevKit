*core
baseOverridesCoreSequenceableCollection: known

	self 
		addTo: known 
		forBehavior: SequenceableCollection 
		overrides: {
			#first -> 'first

"Returns the first element of the receiver.  Generates an error if the receiver
 is empty."

(self size == 0)
ifTrue: "attempt to access an empty Collection"
   [ ^ self _error: #objErrCollectionEmpty].
^ self at: 1
'.
	}