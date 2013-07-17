*change-notification
renameOrMergeCategory: oldName to: newName

"Changes the name of the specified category to newName (a String), and
 returns the receiver.  If oldName is not in the receiver's method
 dictionary, generates an error.  If newName is already in the receiver's
 category list, moves all the methods from the old category to the new
 category, and removes the old category."

| sels oldsym newsym seg canWrite |

self _validatePrivilege.
oldsym := oldName asSymbol.
sels := self _categories at: oldsym otherwise: nil.
sels == nil ifTrue: [
  ^ self _error: #classErrMethCatNotFound args: #[oldName] ].  "key not found"

seg := self segment.
canWrite := (seg == nil
  ifTrue: [ true ]
  ifFalse: [ seg currentUserCanWrite ]).
newsym := newName asSymbol.
canWrite
  ifTrue: [
    (categories includesKey: newsym) ifTrue: [
      sels size downTo: 1 do: [ :i |
        self moveMethod: (sels at: i) toCategory: newsym].
      categories removeKey: oldsym ifAbsent: []]
    ifFalse: [
      (categories includesKey: oldsym) ifTrue: [
        categories add:
    	(SymbolAssociation new key: newsym value: (categories at: oldsym)).
        categories removeKey: oldsym]]]
  ifFalse: [
    (self _categories includesKey: newsym) ifTrue: [
        (self _categories at: oldsym) do: [:selectorSym |
          self moveMethod: selectorSym toCategory: newsym]]
      ifFalse: [
        (self _categories includesKey: oldsym) ifTrue: [ | oldCatSet catSet |
          GsPackagePolicy current addCategory: newsym for: self.
          (self _categories at: oldsym) do: [:selectorSym | | meth |
   		    GsPackagePolicy current addSelector: selectorSym toCategory: newsym for: self.
			meth := self compiledMethodAt: selectorSym.
			SystemChangeNotifier uniqueInstance 
				announce: (MethodMovedAnnouncement itemClass: self method: meth oldCategory: oldsym)]]].
    GsPackagePolicy current removeCategory: oldsym for: self.
    categories removeKey: oldsym ifAbsent: []].
	SystemChangeNotifier uniqueInstance 
		announce: (ClassReorganizedAnnouncement itemClass: self)