*change-notification
moveMethod: aSelector toCategory: categoryName
  | aSel meth oldCat policy newCat |
  aSel := aSelector asSymbol.
  newCat := categoryName asSymbol.
  policy := GsPackagePolicy current.
  oldCat := policy categoryOfSelector: aSel for: self.
  oldCat
    ifNil: [ self moveMethod: aSel toCategory: newCat environmentId: 0 ]
    ifNotNil: [ policy moveSelector: aSel toCategory: newCat for: self ].
  meth := self compiledMethodAt: aSelector.
  SystemChangeNotifier uniqueInstance
    announce:
      (MethodMovedAnnouncement itemClass: self method: meth oldCategory: oldCat)