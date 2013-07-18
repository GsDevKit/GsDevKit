testing
testEncodeForHTTP
	self assert: (Character withValue: 246) asString encodeForHTTP = '%C3%B6'