private-actions
transactionBoundary: transactionSymbol

	((self cachedClassChangeCount == classChangeCount) and: [self cachedSessionMethodChangeCount == sessionMethodChangeCount]) 
		ifTrue: [ "no change, so no need to refresh caches" ^self ].
	"clear cacheOrganizer cache"
	self cachedClassChangeCount ~~ classChangeCount 
		ifTrue: [ self refreshOrganizerCache ].
	"rebuild sessionMethod dictionary"
	self cachedSessionMethodChangeCount ~~ sessionMethodChangeCount 
		ifTrue: [self refreshSessionMethodDictionary].
	self refreshCache
