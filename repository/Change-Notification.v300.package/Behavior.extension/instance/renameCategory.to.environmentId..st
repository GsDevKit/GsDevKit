*change-notification
renameCategory: categoryName to: newCategoryName environmentId: envId

"Changes the name of the specified category to newCategoryName (a
 String), and returns the receiver.  If categoryName is not in the
 receiver's method dictionary, or if newCategoryName is already in the
 receiver's method dictionary, generates an error."

| catSym newCatSym oldCatSet |
self _validatePrivilege ifFalse:[ ^ nil ].
(self includesCategory: newCategoryName environmentId: envId ) ifTrue:[
  ^ self _error: #classErrMethCatExists args: { newCategoryName . self }
].
(self includesCategory: categoryName environmentId: envId ) ifFalse:[
   ^ self _error: #classErrMethCatNotFound args: { categoryName }
].
newCatSym := newCategoryName asSymbol .
catSym := categoryName asSymbol.

(envId ~~ 0 or:[ self canWriteMethods]) ifTrue:[ | catDict|
   catDict :=  self _baseCategorysForStore: envId .
   catDict at: newCatSym put: (oldCatSet := catDict at: catSym ).
   catDict removeKey: catSym .
] ifFalse: [   | pkgCats catSet |
   pkgCats := GsMethodDictionary new  .
   GsPackagePolicy current copyCategoryDictFor: self into: pkgCats .
   oldCatSet := pkgCats at: catSym otherwise: { } .
   catSet := self addCategory: newCatSym environmentId: envId .
   catSet addAll: oldCatSet.
   GsPackagePolicy current removeCategory: catSym for: self.
].
self _needsAnnouncement ifTrue:[
  oldCatSet do: [:sel | 
     self _announceMethodMoved: (self compiledMethodAt: sel) oldCategory: catSym
  ].
].
