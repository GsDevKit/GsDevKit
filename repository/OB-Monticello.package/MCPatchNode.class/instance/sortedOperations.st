as yet unclassified
sortedOperations
	^ self operations sortBy: [:a :b | a theClass name <= b theClass name ]