accessing
initializeWithTree: t1 
	t1 isSymbol
		ifTrue: [name := t1.
			children := Array new.
			hasResolution := true]
		ifFalse: [name := t1 first.
			t1 second isSymbol
				ifTrue: [hasResolution := false.
					children := Array new]
				ifFalse: [hasResolution := true.
					children := t1 second]]