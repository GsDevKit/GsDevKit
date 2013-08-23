private
requestClassToUser
	| pattern path env class symDictNode |
	pattern := OBTextRequest 
					prompt: 'Please type the name or fragment to look for' 
					template: ''.
	pattern ifNil: [^nil].
	env := target environment.
	(path := requestor browser navigationPanel selectionPath) notEmpty
		ifTrue: [ 
			path first isSymbolDictionaryNode 
				ifTrue: [ 
					symDictNode := env := path first ]].
	class := self findClassIn: env pattern: pattern.
	(symDictNode notNil and: [ class notNil ])
		ifTrue: [ 
			^(class asNode)
				symbolDictionary: symDictNode symbolDictionary;
				yourself ].
	^class