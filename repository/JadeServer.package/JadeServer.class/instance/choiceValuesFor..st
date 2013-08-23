OmniBrowser
choiceValuesFor: anOBChoiceRequest

	^self stringFromCollection: (anOBChoiceRequest values collect: [:ea | 
		self addToPureExportSet: ea.
		ea asOop printString ])