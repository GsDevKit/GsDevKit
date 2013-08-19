as yet unclassified
addVersion: aVersion

	MCPlatformSupport 
		writeStreamForFileNamed: (directory fullNameFor: aVersion fileName asString) 
		onClient: false 
		replace: true 
		do: [:file | aVersion fileOutOn: file ].