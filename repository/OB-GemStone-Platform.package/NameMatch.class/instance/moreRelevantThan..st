as yet unclassified
moreRelevantThan: aNameMatch
	equals ifTrue: [^true].
	aNameMatch equals ifTrue: [^false].
	findStringNoCaseIndex = 0 ifTrue: [^false].
	aNameMatch findStringNoCaseIndex = 0 ifTrue: [^true].
	^findStringNoCaseIndex = aNameMatch findStringNoCaseIndex
		ifTrue: [findStringIndex = aNameMatch findStringIndex
			ifTrue: [name < aNameMatch name]
			ifFalse: [findStringIndex < aNameMatch findStringIndex]]
		ifFalse: [findStringNoCaseIndex < aNameMatch findStringNoCaseIndex]