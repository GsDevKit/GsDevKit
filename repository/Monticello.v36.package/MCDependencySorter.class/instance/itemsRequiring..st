private
itemsRequiring: anObject
	anObject = 'nil'
		ifTrue: [ self halt ].
	^ required at: anObject ifAbsentPut: [ Set new ]