navigation
columnAfter: anOBColumn withFan: anOBFan selection: aSmallInteger 
	| navigation |
	navigation := anOBFan anyNavigation.
	anOBFan restrictToNavigation: navigation.
	^ navigation columnAfter: anOBColumn withFan: anOBFan selection: aSmallInteger.