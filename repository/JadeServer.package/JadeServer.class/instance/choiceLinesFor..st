OmniBrowser
choiceLinesFor: anOBChoiceRequest

	^self stringFromCollection: (anOBChoiceRequest lines collect: [:ea | ea printString])