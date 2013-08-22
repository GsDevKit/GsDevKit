accessing
selection
	"GemStone-specific implementation"
	
	| start |
	start := (self source asString findString: message startingAt: 1).
	^ start to: start + message size - 1