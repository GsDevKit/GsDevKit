OmniBrowser
panelInfoFor: anOBBrowser

	| response oop class |
	response := OrderedCollection new.
	oop := false asOop.
	class := nil asOop.
	#(#MBOBConfigurationBrowser #MDConflictBrowser #OSTestRunner #OTBasicInspector  #OTChasingInspector #OTWorkspace)
		do: [:sym | | targetClass |
			targetClass := Smalltalk at: sym ifAbsent: [].
			(targetClass ~~ nil and: [ anOBBrowser isKindOf: targetClass ]) 
				ifTrue: [
					oop := true asOop. 
					class := anOBBrowser class asOop]].
	response add: ([anOBBrowser wantsStepsIn asOop printString] on: Error do: [:ex | false asOop printString ]).
	response add: oop printString; add: class printString.
	anOBBrowser panels do: [:panel | response add: panel asOop printString; add: panel class asOop printString ].
	^self stringFromCollection: response