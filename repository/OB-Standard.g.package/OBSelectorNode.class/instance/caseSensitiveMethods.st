navigating
caseSensitiveMethods
	methods ifNil: 
		[methods := SystemNavigation default allMethodsNoDoitsSelect: [:m | 
			(m _sourceString findString: selector asString startingAt: 1 caseSensitive: true) > 0]].
	^ methods collect: [:m | OBMethodNode on: m]