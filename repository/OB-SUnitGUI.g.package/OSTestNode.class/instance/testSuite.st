accessing
testSuite

	| suite |
	suite := TestSuite new.
	self coParents do: [:aParent |
		aParent theClass isAbstract ifFalse: [ aParent theClass addToSuiteFromSelectors: suite ]].
	^suite