instance creation
parseDocumentFrom: aStream
	^self parseDocumentFrom: aStream useNamespaces: false persistentRoot: nil