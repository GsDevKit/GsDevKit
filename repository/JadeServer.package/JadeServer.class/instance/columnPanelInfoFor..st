OmniBrowser
columnPanelInfoFor: anOBColumnPanel

	| response |
	response := OrderedCollection new.
	anOBColumnPanel columns do: [:column | 
		response 
			add: column asOop printString;
			add: column class asOop printString ].
	^self stringFromCollection: response