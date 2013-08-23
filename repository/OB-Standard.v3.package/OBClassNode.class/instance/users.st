navigating
users
	^ (SystemNavigation default allReferencesTo: (theClass name))
		collect: [:ref | OBClassRefNode on: self name inMethod: ref]