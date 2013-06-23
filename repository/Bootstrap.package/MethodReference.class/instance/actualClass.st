queries
actualClass 

	| actualClass |

	actualClass := (System myUserProfile symbolList objectNamed: classSymbol).
	actualClass == nil ifTrue: [^nil].
	classIsMeta ifTrue: [^actualClass class].
	^actualClass