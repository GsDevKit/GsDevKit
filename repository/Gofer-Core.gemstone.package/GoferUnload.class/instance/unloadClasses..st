unloading
unloadClasses: aWorkingCopy
	aWorkingCopy packageInfo methods do: [ :each |
		(each classIsMeta and: [ each methodSymbol = #unload ])
			ifTrue: [ each actualClass theNonMetaClass unload ] ]