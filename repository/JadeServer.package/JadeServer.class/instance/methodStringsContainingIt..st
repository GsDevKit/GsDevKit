menu messages
methodStringsContainingIt: aString

	OBMethodStringsBrowser browseRoot: (OBSelectorNode on: aString)
		target: nil