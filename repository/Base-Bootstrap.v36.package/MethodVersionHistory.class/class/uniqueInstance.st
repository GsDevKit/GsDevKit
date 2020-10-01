accessing
uniqueInstance	
	| dictionary ui |
	dictionary := self storage.	
	ui := dictionary at: #MethodVersionHistory_UniqueInstance otherwise: nil.
	ui == nil 
		ifTrue: [
			ui := self new.
			dictionary at: #MethodVersionHistory_UniqueInstance put: ui].
	^ui