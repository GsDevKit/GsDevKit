*ob-standard
browseMethodsWithString: aString

	OBMethodStringsBrowser 
		browseRoot: (OBSelectorNode on: aString)
		target: nil