execution
execute

	| pattern filter |
	(filter :=  self filter) ifNil: [^self ].
	self clearTestCases.
	pattern := OBTextRequest 
		prompt: 'Pattern to select categories:'
		template: (filter pattern ifNil: [ '*' ]).
	filter pattern: pattern.
	self update