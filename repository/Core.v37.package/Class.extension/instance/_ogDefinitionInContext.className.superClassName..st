*core-squeak-subclass creation
_ogDefinitionInContext: aUserProfile className: className superClassName: superClassName
	"Returns a description of the receiver using object names taken from the given
 UserProfile. Crafted for OG-Standard browsers"

	| result anArray lfsp newByteSubclass firstElement poolDicts civs |
	result := String new.
	result addAll: superClassName.
	lfsp := Character lf asString
		addAll: String tab;
		yourself.
	newByteSubclass := false.
	(self isBytes _and: [ superClass isBytes not ])
		ifTrue: [ 
			result
				addAll: ' byteSubclass: ''';
				addAll: className;
				addLast: $'.
			newByteSubclass := true ]
		ifFalse: [ 
			self _portableIsIndexable
				ifTrue: [ 
					result
						addAll: ' indexableSubclass: ''';
						addAll: className;
						addLast: $' ]
				ifFalse: [ 
					result
						addAll: ' subclass: ''';
						addAll: className;
						addLast: $' ] ].	" instVarNames: #( <list of strings> ) "
	newByteSubclass
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #(';
				addAll: (self _instVarNamesWithSeparator: lfsp , '                 ');
				add: $) ].	" classVars: #( <list of strings> ) "
	result
		addAll: lfsp;
		addLast: 'classVars: #('.
	self _sortedClassVarNames
		do: [ :aKey | 
			result addLast: $ .
			(aKey includesValue: $')
				ifTrue: [ result addAll: aKey _asSource ]
				ifFalse: [ result addAll: aKey ] ].
	result addLast: $).	" classInstVars: #( <list of strings> ) "
	result
		addAll: lfsp;
		addLast: 'classInstVars: #('.
	civs := self class allInstVarNames.
	civs removeFrom: 1 to: self class superClass instSize.
	civs
		do: [ :civName | 
			result addLast: $ .
			(civName includesValue: $')
				ifTrue: [ result addAll: civName _asSource ]
				ifFalse: [ result addAll: civName ] ].
	result addLast: $).	" poolDictionaries: { <list of dictionary names> } "
	result
		addAll: lfsp;
		addAll: 'poolDictionaries: '.
	(poolDicts := self sharedPools) size > 0
		ifTrue: [ 
			result addAll: ' {'.
			firstElement := true.
			poolDicts
				do: [ :each | 
					firstElement
						ifFalse: [ result addAll: ' . ' ].
					(anArray := aUserProfile dictionaryAndSymbolOf: each)
						ifNil: [ 
							| d |
							(d := aUserProfile objectNamed: each name)
								ifNotNil: [ 
									((d isKindOf: Class) and: [ d isSubclassOf: SharedPool ])
										ifTrue: [ result addAll: each name , ' _classVars' ]
										ifFalse: [ result addAll: '"(not named)"' ] ]
								ifNil: [ result addAll: '"(not named)"' ] ]
						ifNotNil: [ result addAll: (anArray at: 2) ].
					firstElement := false ].
			result addAll: ' }' ]
		ifFalse: [ result addAll: '#()' ].	"inDictionary: <name of Dictionary>"
	result
		addAll: lfsp;
		addAll: 'inDictionary: '.
	anArray := aUserProfile dictionaryAndSymbolOf: self.
	(anArray == nil
		or: [ (anArray at: 1) == GsPackagePolicy current homeSymbolDict ])
		ifTrue: [ result addAll: '''''' ]
		ifFalse: [ 
			(anArray := aUserProfile dictionaryAndSymbolOf: (anArray at: 1))
				ifNil: [ result addAll: '''''' ]
				ifNotNil: [ result addAll: (anArray at: 2) asString printString ] ].	"category: <name of class category>"
	result
		addAll: lfsp;
		addAll: 'category: '.
	result addAll: self category printString.	" constraints: omitted, no longer enforced in Gs64 "	"options:"
	result
		add: lfsp;
		add: self _optionsStringForDefinition.
	result add: Character lf.
	^ result