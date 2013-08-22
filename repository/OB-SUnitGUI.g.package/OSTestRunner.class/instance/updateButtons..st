updating
updateButtons: ann

	| buttonBar |
	buttonBar := panels detect: [:each | each isKindOf: OBFixedButtonPanel ] ifNone: [].
	buttonBar ~~ nil ifTrue: [ buttonBar selectionChanged: nil ].
	(ann column isKindOf: OSMultipleSelectionColumn) 
		ifTrue: [ self signalRefresh ].
	ann node == nil ifTrue: [ ^self ].
	ann node isTestCaseNode
		ifTrue: [ (OSCmdDebugTestCase on: ann node for: self) execute ].