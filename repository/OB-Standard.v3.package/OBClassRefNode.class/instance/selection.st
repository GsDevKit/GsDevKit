accessing
selection
	"GemStone-specific implementation"
	| start |
	start := (self source asString findString: className startingAt: 1).
	^ start to: start + className size - 1