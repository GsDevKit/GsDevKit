running
execute: aWorkingCopy
	aWorkingCopy packageInfo methods do: [ :each |
		(each classIsMeta and: [ each methodSymbol = #initialize ])
			ifTrue: [ each actualClass theNonMetaClass initialize ] ]