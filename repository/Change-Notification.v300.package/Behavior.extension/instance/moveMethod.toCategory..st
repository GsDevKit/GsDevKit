*change-notification
moveMethod: aSelector toCategory: categoryName
    | aSel meth oldCat policy newCat |
  aSel := aSelector asSymbol .
  newCat := categoryName asSymbol .
  oldCat := (policy := GsPackagePolicy current) categoryOfSelector: aSel for: self.
  oldCat ifNil:[
    (self canBeWritten and:[ (self _baseCategorysForStore: 0) canBeWritten ]) ifFalse:[
       ^ self error:'current category is not writable'.
    ].
    oldCat := self categoryOfSelector: aSelector.
    oldCat ifNil:[ self error:'selector not found' ].
    self moveMethod: aSel toCategory: newCat environmentId:0.
  ] ifNotNil:[
    policy moveSelector: aSel toCategory: newCat for: self .
  ].
  meth := self compiledMethodAt: aSelector.
  SystemChangeNotifier uniqueInstance 
    announce: (MethodMovedAnnouncement itemClass: self method: meth oldCategory: oldCat).
