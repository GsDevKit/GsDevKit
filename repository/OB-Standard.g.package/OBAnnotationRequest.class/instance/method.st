accessing
method
	class == nil ifTrue: [ ^nil ].
	^ class compiledMethodAt: selector ifAbsent: [nil]