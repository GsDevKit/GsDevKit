accessing
compiledMethod

	^target theClass 
		compiledMethodAt: target selector 
		ifAbsent: [ nil ]