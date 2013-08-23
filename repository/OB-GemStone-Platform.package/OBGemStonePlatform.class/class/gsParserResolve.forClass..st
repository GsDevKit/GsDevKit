shout
gsParserResolve: aString forClass: classOrMetaClass
	self hasInterned: aString
		ifTrue: [:sym | 
			classOrMetaClass isBehavior 
				ifTrue: 
					[classOrMetaClass theNonMetaClass withAllSuperclasses do: 
							[:c | 
							(c allClassVarNames includes: sym) ifTrue: [^ #classVar].
							c allSharedPools do: [:p | (p bindingOf: sym) ifNotNil: [^ #poolConstant]].
							(c environment bindingOf: sym) ifNotNil: [^ #globalVar]]]
				ifFalse: [(self bindingOf: sym) ifNotNil: [^ #globalVar]]].
	^nil