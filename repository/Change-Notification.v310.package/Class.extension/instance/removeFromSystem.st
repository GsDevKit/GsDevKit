*change-notification
removeFromSystem
	"Remove class from system, answer true if successful and 
	 false if the receiver has subclases."

	| ar undefinedSymbols |
	self subclasses isEmpty not ifTrue: [ ^false ].
	ar := System myUserProfile dictionaryAndSymbolOf: self.
	ar == nil ifTrue: [ ^false ]. "precautionary measure"
	(ar at: 1) removeKey: (ar at: 2).
	undefinedSymbols := GsSession currentSession symbolList objectNamed: #UndefinedSymbols.
	undefinedSymbols ~~ nil
		ifTrue: [ | sym |
			sym := self name asSymbol.
			(ClassOrganizer cachedOrganizer allReferencesTo: sym) first do: [:meth |
				self _addUndefinedSymbol: sym to: undefinedSymbols for: meth ]].
	ClassOrganizer clearCachedOrganizer.
	SystemChangeNotifier uniqueInstance 
		announce: (ClassRemovedAnnouncement itemClass: self oldCategory: self category).
	MethodVersionHistory uniqueInstance removeVersionsForClass: self.
	^true