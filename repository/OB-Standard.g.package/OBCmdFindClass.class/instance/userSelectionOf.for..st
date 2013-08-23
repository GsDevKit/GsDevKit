private
userSelectionOf: classNames for: toMatch 
	| exactMatch |
	exactMatch := classNames detect: [:ea | ea asLowercase = toMatch]
				ifNone: [nil].
	^ exactMatch ifNil: 
			[OBChoiceRequest labels: classNames
				lines: #()]
		ifNotNil: 
			[OBChoiceRequest labels: (classNames copyWithFirst: exactMatch)
				lines: #(1 )]