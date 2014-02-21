parsing
absoluteFromText: aString
	| schemeName locator |
	schemeName _ Url schemeNameForString: aString.
	schemeName ifNil: [ ^self schemeName: 'xnoscheme' locator: aString ].
	locator _ aString copyFrom: (schemeName size + 2) to: aString size.
	^self schemeName: schemeName locator: locator