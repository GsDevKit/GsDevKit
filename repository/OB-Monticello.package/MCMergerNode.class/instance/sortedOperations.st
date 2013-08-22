as yet unclassified
sortedOperations
	^ self operations sortBy: [:a :b | a name <= b name ]