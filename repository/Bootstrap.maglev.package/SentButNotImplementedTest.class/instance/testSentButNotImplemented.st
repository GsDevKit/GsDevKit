tests
testSentButNotImplemented
	| sentNotImplemented |
	(sentNotImplemented := self sentButNotImplemented) removeAllPresent: self specialCases.
	self assert: sentNotImplemented isEmpty