asserting
assertContentsOf: strm match: expected 
	| actual |
	actual := strm contents withSqueakLineEndings.
	self assert: actual size = expected size.
	actual with: expected withSqueakLineEndings do: [:a :e | self assert: a = e]