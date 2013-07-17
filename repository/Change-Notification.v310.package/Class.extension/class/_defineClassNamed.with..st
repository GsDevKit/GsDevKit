*change-notification
_defineClassNamed: aClassName with: aBlock
	| originalClass classOrganizer cls |
	originalClass := System myUserProfile symbolList objectNamed: aClassName asSymbol.
	originalClass ~~ nil 
		ifTrue: [ 
			"pre-create the classOrganizer, so that original subclasses are preserved."
			classOrganizer := ClassOrganizer cachedOrganizer ].
	cls := aBlock value.
	cls == nil
		ifTrue: [ ^nil ].
	originalClass ~~ nil
		ifTrue: [
			SystemChangeNotifier uniqueInstance doSilently: [
				cls 
					copyMCMethodsFrom: originalClass 
					dictionaries: GsSession currentSession symbolList.
				"force redefinition of existing subclasses"
				MCPlatformSupport redefineSubclassesOf: originalClass using: classOrganizer.
				MCPlatformSupport migrateInstancesWithSubclassesOf: cls ].
			cls category = originalClass category
				ifTrue: [
					SystemChangeNotifier uniqueInstance 
						announce: (ClassModifiedAnnouncement itemClass: cls oldCategory: originalClass category)]
				ifFalse: [
					SystemChangeNotifier uniqueInstance 
						announce: (ClassMovedAnnouncement itemClass: cls oldCategory: originalClass category)]]
		ifFalse: [
			SystemChangeNotifier uniqueInstance 
				announce: (ClassAddedAnnouncement itemClass: cls)].
	SystemChangeNotifier uniqueInstance doSilently: [ Class _resolveUndeclaredSymbolsForClass: cls ].
	^cls