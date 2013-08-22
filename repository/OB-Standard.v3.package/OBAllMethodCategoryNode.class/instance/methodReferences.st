accessing
methodReferences
	^ self theClass selectors asSortedArray 
		collect: [:ea | 
			(MethodReference new setStandardClass: self theClass methodSymbol: ea)
				symbolDictionary: self symbolDictionary;
				yourself ]