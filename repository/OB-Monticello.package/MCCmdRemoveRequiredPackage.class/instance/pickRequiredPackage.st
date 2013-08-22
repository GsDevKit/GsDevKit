actions
pickRequiredPackage
	| required |
	required  := target workingCopy requiredPackages.
	required isEmpty ifTrue: [ ^nil ].
	^OBChoiceRequest 
		prompt: 'Package:' 
		labels: (required collect: [:ea | ea name])
		values: required.