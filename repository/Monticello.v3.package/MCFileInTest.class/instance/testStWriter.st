testing
testStWriter
	<timeout: 30> "takes a little longer"
	self
		assertFileOutFrom: MCStWriter
		canBeFiledInWith: [stream fileIn].
