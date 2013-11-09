private
requestClassToUser
	|classeNames|
	classeNames := OBRecentClasses default classes.
	^ classeNames isEmpty
		ifTrue: [nil]
		ifFalse: [OBChoiceRequest
					prompt: nil
					labels: classeNames
					values: (classeNames collect: [:name | Smalltalk at: name ifAbsent: nil])]
	