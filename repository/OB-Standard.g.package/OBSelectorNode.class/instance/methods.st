navigating
methods
	methods ifNil: 
		[methods := SystemNavigation default allMethodsNoDoitsSelect: [:m | 
			(m _sourceString findString: selector asString startingAt: 1 caseSensitive: false) > 0]].
	^ methods collect: [:m | OBMethodNode on: m]