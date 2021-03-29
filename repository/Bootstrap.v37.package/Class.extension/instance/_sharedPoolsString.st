*bootstrap
_sharedPoolsString

	| result firstElement aUserProfile |
	result := String new.
	firstElement := true.
	aUserProfile := System myUserProfile.
	self sharedPools do: [:each | | anArray |
  		firstElement ifFalse: [ result addAll: ', '].  "separate with commas"
  		anArray := aUserProfile dictionaryAndSymbolOf: each.
  		anArray == nil
        		ifTrue: [ | d |
            		(d := aUserProfile objectNamed: each name) ~~ nil 
					ifTrue: [ 
						((d isKindOf: Class) and: [d isSubclassOf: SharedPool])
							ifTrue: [ result addAll: ( each name, ' _classVars')  ]
							ifFalse: [ result addAll: ' "(not named)" ' ]]
					ifFalse: [result addAll: ' "(not named)" ' ]]
        		ifFalse: [ result addAll: (anArray at: 2)].
  		firstElement := false].
	^result