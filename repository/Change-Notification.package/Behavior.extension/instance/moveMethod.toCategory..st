*change-notification
moveMethod: aSelector toCategory: categoryName

"Moves the method aSelector (a String) from its current category to the
 specified category (also a String).  If either aSelector or categoryName is
 not in the receiver's method dictionary, or if aSelector is already in
 categoryName, generates an error."

| theKeys index oldCateg newCateg theSize selectorSym oldCat seg canWrite meth |

	"this method does not account for selectors inherited from superclasses"

	self _validatePrivilege.
	oldCat := self categoryOfSelector: aSelector.

	theKeys:= (Array withAll: (categories keys)).
	theSize := theKeys size.
	selectorSym := aSelector asSymbol .
	index:= 1.  "find original category of aSelector"

	seg := self segment.
	canWrite := (seg == nil
		ifTrue: [ true ]
		ifFalse: [ seg currentUserCanWrite ]).
	canWrite
  		ifTrue: [
			[((oldCateg:= (categories at: (theKeys at: index)))
                            includesValue: selectorSym )
  				or: [index >= theSize]] whileFalse: [index := index + 1].

			(oldCateg includesValue: selectorSym ) 
  				ifTrue: [ oldCateg remove: selectorSym ]
 				ifFalse: [ ^ self _error: #classErrSelectorNotFound args: #[selectorSym] ].

    			newCateg:= categories at: categoryName asSymbol otherwise: nil.
    			(newCateg includesValue: selectorSym) "add aSelector"
    				ifTrue: [
      				"selector already there, unlikely"
      				self _uncontinuableError]
    				ifFalse: [newCateg add: selectorSym]]
  		ifFalse: [
   			GsPackagePolicy current removeSelector: selectorSym fromCategoriesFor: self.
            "The category may not exist already"
   			GsPackagePolicy current addCategory: categoryName asSymbol for: self.
   			GsPackagePolicy current addSelector: selectorSym toCategory: categoryName asSymbol for: self].
	meth := self compiledMethodAt: aSelector.
	SystemChangeNotifier uniqueInstance 
		announce: (MethodMovedAnnouncement itemClass: self method: meth oldCategory: oldCat)
