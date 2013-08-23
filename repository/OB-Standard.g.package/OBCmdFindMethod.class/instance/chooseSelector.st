private
chooseSelector
	| selectors |
	selectors := target theClass selectors asSortedArray.
	^ selectors isEmpty 
		ifTrue: [nil]
		ifFalse: [OBChoiceRequest labels: selectors]