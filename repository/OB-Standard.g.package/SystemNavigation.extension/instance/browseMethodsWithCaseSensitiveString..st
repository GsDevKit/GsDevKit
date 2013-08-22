*ob-standard
browseMethodsWithCaseSensitiveString: aString

	OBMethodCaseSensitiveStringsBrowser 
		browseRoot: (OBSelectorNode on: aString)
		target: nil