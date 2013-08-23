execution
execute
	| labels values cls |
	labels := Array with: (self menuLabelForClass: target theClass) asString.
	values := Array with: target theClass.
	target theClass allSuperclassesDo: 
			[:each | 
			labels add: (self menuLabelForClass: each) asString.
			values add: each].
	cls := OBChoiceRequest
		prompt: self menuString 
		labels: labels
		values: values.
	cls ~~ nil ifTrue: [ self limitClass: cls ].