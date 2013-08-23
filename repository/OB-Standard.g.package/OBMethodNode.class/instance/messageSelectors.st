private
messageSelectors
	| method |
	method := self theClass compiledMethodAt: self selector ifAbsent: [^ #()].
	^method _selectorPool asSortedArray