OmniBrowser
commandPanelInfoFor: aFixedButtonPanel

	| response |
	response := OrderedCollection new.
	aFixedButtonPanel commands do: [:command | 
		self addToPureExportSet: command.
		response add: command asOop printString ].
	^self stringFromCollection: response