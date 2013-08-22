actions
pickWorkingCopySatisfying: aBlock
	| workingCopies |
	workingCopies := self workingCopies select: aBlock.
	workingCopies isEmpty ifTrue: [ ^nil ].
	^OBChoiceRequest 
		prompt: 'Package:' 
		labels: (workingCopies collect: [:ea | ea packageName])
		values: workingCopies.