tests
testEquality
	| standard mc |
	standard := 'Package-author.123.mcz'.
	mc := standard asMCVersionName.
	self
		assert: mc = standard;
		"assert: standard = mc;"
		yourself.
	self flag: #review. "comparing the string to the name does not work on GS due to mc being a ProtoObject"
	self assert: self allValidFixtures asSet size = 1