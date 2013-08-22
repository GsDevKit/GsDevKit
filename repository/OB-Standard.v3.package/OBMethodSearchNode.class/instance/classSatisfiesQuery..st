as yet unclassified
classSatisfiesQuery: aClass
	^ aClass selectors anySatisfy: [:ea | ea includesSubstring: query caseSensitive: true]