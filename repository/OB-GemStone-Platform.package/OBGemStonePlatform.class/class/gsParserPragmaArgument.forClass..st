shout
gsParserPragmaArgument: aString forClass: classOrMetaClass
	Symbol hasInterned: aString
		ifTrue: [:sym | 
			classOrMetaClass isBehavior 
				ifTrue: [classOrMetaClass theNonMetaClass withAllSuperclasses 
						do: [:c | (c environment bindingOf: sym) ifNotNil: [^ #globalVar]]]
				ifFalse: [(self bindingOf: sym) ifNotNil: [^ #globalVar]]].
^nil