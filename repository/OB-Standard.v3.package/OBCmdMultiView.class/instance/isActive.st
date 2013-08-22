testing
isActive

	self flag: #portToGemStone.
	^false
	"^ (target isKindOf: OBMethodNode)
		and: [requestor isSelected: target]"