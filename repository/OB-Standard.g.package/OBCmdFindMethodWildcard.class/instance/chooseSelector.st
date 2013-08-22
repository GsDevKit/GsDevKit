private
chooseSelector
	| aClass selectors reply |
	aClass := target theClass.
	selectors := aClass selectors asSortedArray.
	selectors isEmpty ifTrue: [^ nil].
	reply := OBTextRequest prompt: 'Enter partial method name:' template: '*'.
	(reply isNil or: [reply isEmpty]) ifTrue: [^ nil].
	(reply includes: $*) ifFalse: [reply := '*' , reply , '*'].
	selectors := selectors select: [:each | reply match: each].
	selectors isEmpty ifTrue: [^ nil].
	reply := selectors size = 1 
				ifTrue: [selectors first]
				ifFalse: [OBChoiceRequest labels: selectors].
	^ reply