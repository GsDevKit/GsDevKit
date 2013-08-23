updating
announceSelectionWith: anObject
	(anObject announce: OBAboutToChange)
		isVetoed ifFalse: [anObject announce: (OBSelectingNode node: self)]