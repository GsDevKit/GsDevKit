initializing
initializeCommands
	
	cmdFactories _ Array streamContents: 
		[:stream | 
		(self commandSelectors) do: 
			[:selector | 
			| result |
			result := self perform: selector.
			result isCollection 
				ifTrue: [stream nextPutAll: result]
				ifFalse: [stream nextPut: result]]].