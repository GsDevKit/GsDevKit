as yet unclassified
readStreamForFileNamed: aString do: aBlock
	^MCPlatformSupport readStreamForFileNamed: (directory fullNameFor: aString asString) onClient: self onClient do: aBlock