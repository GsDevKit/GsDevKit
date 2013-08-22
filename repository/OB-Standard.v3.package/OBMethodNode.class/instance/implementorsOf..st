private
implementorsOf: aSelector
	^ (SystemNavigation default allImplementorsOf: aSelector) asSortedArray
			collect: [:ref | (OBMethodNode on: ref) symbolDictionary: self symbolDictionary; yourself]