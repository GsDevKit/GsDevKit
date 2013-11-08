*ob-gemstone-platform-GSExtensions
findClassNamesMatching: aString
	| matches |
	self collectClasses.
	matches := Set new.
	classes do: [ :aClass |
			(aClass name includesString: aString)
				ifTrue: [matches add: (NameMatch new name: aClass name matching: aString)]].
	^self sortNameMatchesAsString: matches
	