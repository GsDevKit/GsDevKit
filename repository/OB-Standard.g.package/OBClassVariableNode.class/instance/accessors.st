navigating
accessors
	| literal |
	literal := (self theClass withAllSuperclasses 
				gather: [:ea | ea classPool associations])
					detect: [:ea | ea key = name].
	^ ((self systemNavigation allReferencesTo: literal key) asArray sort)
		collect: [:ref | OBMethodNode on: ref]