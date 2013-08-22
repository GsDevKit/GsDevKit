converting
asMethodNode
	^ (OBMethodNode on: selector inClass: self theClass)
			symbolDictionary: self symbolDictionary;
			yourself