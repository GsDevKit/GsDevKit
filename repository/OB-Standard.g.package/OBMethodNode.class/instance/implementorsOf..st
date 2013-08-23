private
implementorsOf: aSelector
	^ (SystemNavigation default allImplementorsOf: aSelector) asSortedArray
			collect: [:ref | OBMethodNode on: ref]