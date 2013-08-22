updating
signalSelection
	(self announce: OBAboutToChange)
		isVetoed ifFalse: [self announce: (OBSelectingNode node: self)]