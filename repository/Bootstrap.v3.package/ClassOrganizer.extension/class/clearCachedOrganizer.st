*bootstrap-caching
clearCachedOrganizer
	"public message for clearing the cached organizer"
	
	SessionTemps current at: #Cached_Class_Organizer put: nil.
	TransactionBoundaryDefaultPolicy isActive
		ifTrue: [ TransactionBoundaryDefaultPolicy current classChanged].
