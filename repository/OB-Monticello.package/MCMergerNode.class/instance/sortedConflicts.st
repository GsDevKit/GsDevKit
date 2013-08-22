as yet unclassified
sortedConflicts
	^ self conflicts sortBy: [:a :b | a name asString <= b name asString ]