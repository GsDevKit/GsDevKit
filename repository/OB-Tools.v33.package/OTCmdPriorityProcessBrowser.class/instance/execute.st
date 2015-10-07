execution
execute
	| priority |
	priority := (OBTextRequest prompt: 'New priority' template: target process priority asString)
		ifNil: [ ^ self ].
	priority := priority asNumber asInteger.
	(priority between: Processor lowestPriority and: Processor highestPriority)
		ifFalse: [ ^ OBInformRequest message: 'Bad priority' ].
	target process priority: priority.
	self update