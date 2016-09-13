utilities
pragmas: aMethodBody
	^ (self compile: aMethodBody selector: #zork)
		pragmas.