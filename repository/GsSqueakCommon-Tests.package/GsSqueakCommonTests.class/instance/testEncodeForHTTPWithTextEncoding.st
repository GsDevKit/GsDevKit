testing
testEncodeForHTTPWithTextEncoding
	self assert: ((Character withValue: 246) asString encodeForHTTPWithTextEncoding: 'latin-1') = '%F6'.	
	self assert: ((Character withValue: 246) asString encodeForHTTPWithTextEncoding: 'utf-8') = '%C3%B6'