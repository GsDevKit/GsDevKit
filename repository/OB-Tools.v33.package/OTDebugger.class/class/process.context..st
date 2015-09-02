instance-creation
process: aProcess context: aContext 
	| processNode contextNode |
	processNode := OBPlatform current otProcessNodeClass on: aProcess.
	contextNode := OBPlatform current otContextNodeClass on: aProcess context: (aContext
		ifNil: [ aProcess suspendedContext ]).
	^ self root: processNode selection: contextNode