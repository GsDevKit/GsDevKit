navigating
implementors
	^ (SystemNavigation default allImplementorsOf: self selector) asSortedArray
			collect: [:ref | OBMethodNode on: ref]