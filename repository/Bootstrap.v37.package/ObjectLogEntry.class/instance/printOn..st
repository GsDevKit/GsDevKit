formatting
printOn: aStream

	aStream nextPutAll: self priorityString, ' ', self labelString, '(', pid asString, ')', '->', self objectString