private
selectorBlockFor: pattern 
	| toMatch |
	toMatch := (pattern copyWithout: $.) asLowercase.
	^ (pattern endsWith: '.') 
		ifTrue: [[:ea | ea asLowercase = toMatch]]
		ifFalse: [[:ea | ea includesSubstring: toMatch caseSensitive: false]]