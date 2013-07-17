*change-notification
_basicRemoveSelector: aSymbol

"Private."
| cat oldMeth removedFromSelf |
self _validatePrivilege.
removedFromSelf := false.
cat := self categoryOfSelector: aSymbol.
(oldMeth := GsPackagePolicy current removeMethodAt: aSymbol for: self) == nil
	ifTrue: [
		oldMeth := methodDict removeKey: aSymbol . "runtime error here if key not found"
        	self setStamp: nil forMethod: aSymbol.
        	self setPragmas: nil forMethod: aSymbol.
		removedFromSelf := true.
	].
self _refreshLookupCache: aSymbol oldMethod: oldMeth.
SystemChangeNotifier uniqueInstance 
	announce: (MethodRemovedAnnouncement itemClass: self method: oldMeth oldCategory: cat).
MethodVersionHistory uniqueInstance removeVersionsForMethod: oldMeth.
^removedFromSelf
