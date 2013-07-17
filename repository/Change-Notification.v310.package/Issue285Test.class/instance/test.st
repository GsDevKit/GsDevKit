tests
test

	| cl |
	cl := self createTestClassIn: 'Issue-285'.
	self assert: ((ClassOrganizer cachedOrganizer categories at: 'Issue-285') includes: cl).
	cl category: 'XIssue-285'.
	self assert: ((ClassOrganizer cachedOrganizer categories at: 'XIssue-285') includes: cl).
