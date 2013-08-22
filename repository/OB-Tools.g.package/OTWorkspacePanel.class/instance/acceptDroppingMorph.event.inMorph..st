drag and drop
acceptDroppingMorph: aTransferMorph event: anEvent inMorph: aMorph
	| node name |
	node := aTransferMorph passenger.
	name := OBTextRequest
		prompt: 'Choose a variable name:'
		template: node name.
	name isEmptyOrNil ifTrue: [ ^ false ].
	(self bindingOf: name) value: node value.
	^ true