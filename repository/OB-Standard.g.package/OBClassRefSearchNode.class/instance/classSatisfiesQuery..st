as yet unclassified
classSatisfiesQuery: aClass
	^ (SystemNavigation default allReferencesTo: query asSymbol) isEmpty not