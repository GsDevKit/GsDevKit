as yet unclassified
fillInTheBlankConfigure: aTemplateString
	| chunk repo |
	
	aTemplateString ifNil: [ ^ false ].
	chunk := MCPlatformSupport request: self fillInTheBlankRequest 
			initialAnswer: aTemplateString.
			
	chunk 
		ifNotNil: [ 
			repo := self readFrom: chunk readStream.
			repo creationTemplate: chunk. 
	].

	^ repo