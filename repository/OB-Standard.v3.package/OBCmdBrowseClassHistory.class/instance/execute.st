execution
execute
	| answer allClasses |
	allClasses := target theNonMetaClass classHistory.
	allClasses size <= 1 ifTrue: [^OBInformRequest message: 'No class history for ',target theNonMetaClass name asString ,'!'].
	 answer := OBChoiceRequest 
				prompt: 'Select class version'
				labels: (allClasses collect: [:cl | cl name ,' (', (allClasses indexOf: cl) asString,')'])
				values: allClasses.
	answer ifNil: [ ^ nil ].
	OBCodeBrowser defaultHierarchyBrowserClass openOnClass: answer