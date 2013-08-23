testing
isActive
	"may need isMethodCategory for GemStone"
	^ target isMethodCategory and: [requestor isSelected: target]