*change-notification
_basicRemoveSelector: aSymbol environmentId: envId
  "Private."

  | oldMeth removedFromSelf |
  self _validatePrivilege
    ifFalse: [ ^ nil ].
  removedFromSelf := false.
  envId == 0
    ifTrue: [ 
      oldMeth := GsPackagePolicy currentOrNil
        ifNotNil: [ :pp | pp removeMethodAt: aSymbol for: self ].
      oldMeth
        ifNil: [ 
          (self persistentMethodDictForEnv: 0)
            ifNotNil: [ :dict | oldMeth := dict removeKey: aSymbol	"runtime error here if key not found" ].
          self setStamp: nil forMethod: aSymbol.
          removedFromSelf := true ].
      self _codeChangedForEnv: envId.
      self _refreshLookupCache: aSymbol oldMethod: oldMeth env: envId ]
    ifFalse: [ removedFromSelf := self __basicRemoveSelector: aSymbol environmentId: envId ].
  SystemChangeNotifier uniqueInstance
    announce: (MethodRemovedAnnouncement itemClass: self method: oldMeth).
  MethodVersionHistory uniqueInstance removeVersionsForMethod: oldMeth.
  ^ removedFromSelf