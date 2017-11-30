parsing
privateInitializeFromText: aString
	schemeName _ Url schemeNameForString: aString.
	schemeName ifNil: [ self error: 'opaque URL with no scheme--shouldn''t happen!'. ].
	locator _ aString copyFrom: (schemeName size+2) to: aString size.