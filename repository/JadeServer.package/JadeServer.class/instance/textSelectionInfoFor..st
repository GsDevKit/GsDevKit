OmniBrowser
textSelectionInfoFor: anOBDefinitionPanel

	| interval |
	interval := anOBDefinitionPanel selection.
	^(interval perform: #_from) printString, String tab, 
		(interval perform: #_to) printString, String tab, 
		interval increment printString