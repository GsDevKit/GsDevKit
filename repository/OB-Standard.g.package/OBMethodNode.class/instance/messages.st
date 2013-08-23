navigating
messages
	| method |
	method := self theClass
				compiledMethodAt: self selector 
				ifAbsent: [^ #()].
	^ method _selectorPool asSortedArray
					collect: [:sel | OBMessageNode 
									on: sel 
									inMethod: selector 
									inClass: self theClass]