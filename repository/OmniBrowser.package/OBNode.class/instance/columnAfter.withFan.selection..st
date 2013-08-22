navigation
columnAfter: anOBColumn withFan: anOBFan selection: aSmallInteger
	| newIndex |
	anOBFan restrictToNavigation: self navigation.
	newIndex := anOBFan indexOf: self.
	^ self navigation columnAfter: anOBColumn withFan: anOBFan selection: newIndex.