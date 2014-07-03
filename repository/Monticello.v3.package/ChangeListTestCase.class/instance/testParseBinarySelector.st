tests
testParseBinarySelector

	self assert: (ChangeList parseSelector: '<= other
		4 < 5') = '<='.
	self assert: (ChangeList parseSelector: '* other
		self / other') = '*'.
	self assert: (ChangeList parseSelector: '//') = '//'.