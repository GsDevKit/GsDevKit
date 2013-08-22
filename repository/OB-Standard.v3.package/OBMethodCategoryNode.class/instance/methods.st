navigating
methods
	^ self methodReferences collect: [:ref | 
		(ref asNode)
			symbolDictionary: self symbolDictionary;
			yourself ]