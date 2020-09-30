tests
testIssue60

	| d |
	d := IdentityDictionary new.
	d at: #a put: 'b'.
	self assert: (d associations first = (#a -> 'b'))