tests
testKindOfSubclass
	| classes d |
	classes := (Array with: self mockClassA with: String with: Collection with: ByteArray).
	classes do: [:c |
		d :=  c asClassDefinition.
		self assert: d kindOfSubclass = c kindOfSubclass.
	].