tests
testEmbeddedCommentSelector

	self assert: (ChangeList parseSelector: self embeddedComment) = 'irNode'.