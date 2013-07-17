*change-notification
_basicRemoveSelector: aSymbol environmentId: envId

| oldMeth removedFromSelf |
self _validatePrivilege.
removedFromSelf := false.
envId == 0 ifTrue:[
  oldMeth := GsPackagePolicy current removeMethodAt: aSymbol for: self .
  oldMeth == nil ifTrue: [
    oldMeth := (self persistentMethodDictForEnv: 0) removeKey: aSymbol . "runtime error here if key not found"
    self setStamp: nil forMethod: aSymbol.
    self setPragmas: nil forMethod: aSymbol.
    removedFromSelf := true.
  ].
  self _refreshLookupCache: aSymbol oldMethod: oldMeth env: envId
] ifFalse:[
  removedFromSelf := self __basicRemoveSelector: aSymbol environmentId: envId .
].
removedFromSelf ifTrue:[
        SystemChangeNotifier uniqueInstance
                announce: (MethodRemovedAnnouncement itemClass: self method: oldMeth).
        MethodVersionHistory uniqueInstance removeVersionsForMethod: oldMeth
].
^removedFromSelf
