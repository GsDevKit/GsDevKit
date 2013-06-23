instance creation
parseDocumentFrom: aStream useNamespaces: aBoolean persistentRoot: persistentArrayOrNil
	|  parser |
	parser _ self on: aStream.
	(persistentArrayOrNil ~~ nil) ifTrue: [ persistentArrayOrNil add: parser ].
	parser useNamespaces: aBoolean.
	parser startDocument.
	parser parseDocument.
	persistentArrayOrNil ~~ nil ifTrue: [ persistentArrayOrNil size: 0 ].
	^parser