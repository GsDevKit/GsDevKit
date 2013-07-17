*change-notification
compileMethod: source category: cat using: aSymbolList
	""
	| result sel oldCat oldSource oldMeth oldStamp |
	sel := Behavior parseSelector: source for: self.
	(sel ~~ nil and: [(oldCat := self categoryOfSelector: sel) ~~ nil])
		ifTrue: [ 
			oldMeth := self compiledMethodAt: sel.
			oldSource := self sourceCodeAt: sel.
			oldStamp := self stampForMethod: sel].
	result := self _compileMethod: source category: cat using: aSymbolList.
	result ~~ nil ifTrue: [ ^result ].
	sel ~~ nil
		ifTrue: [| meth |
			meth := self compiledMethodAt: sel.
			oldCat ~~ nil
				ifTrue: [ 
					(oldCat asString ~= cat asString)
						ifTrue: [
							SystemChangeNotifier uniqueInstance 
								announce: (MethodMovedAnnouncement itemClass: self method: meth oldCategory: oldCat)]
						ifFalse: [
							SystemChangeNotifier uniqueInstance 
								announce: (MethodModifiedAnnouncement itemClass: self method: meth oldCategory: oldCat)]]
				ifFalse: [ 
					SystemChangeNotifier uniqueInstance 
							announce: (MethodAddedAnnouncement itemClass: self method: meth)].
			MethodVersionHistory uniqueInstance addVersionFor: meth oldMeth: oldMeth oldStamp: oldStamp oldCategory: oldCat].
	^nil