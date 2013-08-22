updating
update: aSymbol for: aDependentProxy

	^self shouldRecordUpdates 
		ifTrue: [ self updateStack last add: (aSymbol -> (self reverseDependentsMap keyAtValue: aDependentProxy)) ]
		ifFalse: [ (self dependentsMap at: aDependentProxy ifAbsent: [ ^aDependentProxy ]) update: aSymbol ]