*change-notification
renameCategory: categoryName to: newCategoryName

"Changes the name of the specified category to newCategoryName (a
 String), and returns the receiver.  If categoryName is not in the
 receiver's method dictionary, or if newCategoryName is already in the
 receiver's method dictionary, generates an error."

| catDict |
self _validatePrivilege.
catDict := self _categories.
(catDict includesKey: (newCategoryName asSymbol))
ifTrue: [^ self _error: #classErrMethCatExists args: #[newCategoryName, self]].

(catDict includesKey: (categoryName asSymbol))
ifTrue: [ | seg canWrite |
  seg := self segment.
  canWrite := (seg == nil
    ifTrue: [ true ]
    ifFalse: [ seg currentUserCanWrite ]).
  canWrite
    ifTrue: [
      categories add: (SymbolAssociation newWithKey: (newCategoryName asSymbol)
                                      value: (categories at: categoryName)).
      categories removeKey: categoryName.
    ]
    ifFalse: [ | oldCatSet catSym newCatSym |
      catSym := categoryName asSymbol.
      newCatSym := newCategoryName asSymbol.
	 (categories at: catSym otherwise: nil) ~~ nil ifTrue: [^self perform: #inform: with: 'You do not have authorization to rename category ', categoryName asString printString, ' to ', newCategoryName asString printString ].
      GsPackagePolicy current addCategory: newCatSym for: self.
      (self _categories at: catSym) do: [:selectorSym |
   		GsPackagePolicy current addSelector: selectorSym toCategory: newCatSym for: self].
      GsPackagePolicy current removeCategory: catSym for: self.
      categories removeKey: catSym ifAbsent: [].
    ].
	(self _categories at: newCategoryName asSymbol) do: [:sel | | meth |
		meth := self compiledMethodAt: sel.
		SystemChangeNotifier uniqueInstance 
			announce: (MethodMovedAnnouncement itemClass: self method: meth oldCategory: categoryName asSymbol)]]
ifFalse: [
   "key not found"
   ^ self _error: #classErrMethCatNotFound
            args: #[categoryName]
   ].
SystemChangeNotifier uniqueInstance 
	announce: (ClassReorganizedAnnouncement itemClass: self)