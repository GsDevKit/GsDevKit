accessing
methodReferences
	^ (self theClass selectorsIn: name) asSortedArray
		collect: [:ea | MethodReference new
						setClassSymbol: self theNonMetaClassName
						classIsMeta: self theClass isMeta
						methodSymbol: ea
						stringVersion: '']