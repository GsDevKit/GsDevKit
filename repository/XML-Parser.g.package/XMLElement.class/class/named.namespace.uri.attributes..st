instance creation
named: aString namespace: ns uri: uri attributes: attributeList
	^self new
		name: aString;
		namespace: ns uri: uri;
		setAttributes: attributeList