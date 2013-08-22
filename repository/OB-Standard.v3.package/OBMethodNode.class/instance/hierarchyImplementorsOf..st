private
hierarchyImplementorsOf: aSelector
     | classCollection |
     classCollection := SortedCollection new.
     self theClass withAllSubAndSuperclassesDo: [:class | (class includesSelector: aSelector)
						ifTrue: [
							classCollection add: 
								((MethodReference new setStandardClass: class methodSymbol: aSelector)
									symbolDictionary: self symbolDictionary;
									yourself) ]].
	^classCollection collect: [:ref | ref asNode]