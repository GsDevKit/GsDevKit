*squeak
selectorsAndMethodsDo: aBlock
	"Evaluate selectorBlock for all the message selectors in my method dictionary."

	^ self _methodDict keysAndValuesDo: aBlock