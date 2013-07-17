*change-notification
baseOverridesChangeNotificationBehavior: known
	self
		addTo: known
		forBehavior: Behavior
		overrides: {
			#'_basicRemoveSelector:' -> '_basicRemoveSelector: aSymbol

"Private."
| oldMeth |

self _validatePrivilege.
oldMeth := methodDict removeKey: aSymbol . "runtime error here if key not found"

"refresh method lookup caches to account for removal of the selector
 and delete any breakpoints in the removed method ."
self _refreshLookupCache: aSymbol oldMethod: oldMeth.  
'.
			#'addCategory:' -> 'addCategory: aString

| aSymbol |
"Adds aString as a method category for the receiver.  If aString is already a
 method category, generates an error."

self _validatePrivilege.
aSymbol := aString asSymbol .
(categories includesKey: aSymbol)
ifTrue:[ ^ self _error: #classErrMethCatExists
                  args: #[aString, self]] "category already exists for class"
ifFalse:
   [categories add: (SymbolAssociation newWithKey: aSymbol
                                       value: SymbolSet new ) ]
'.
			#'moveMethod:toCategory:' -> 'moveMethod: aSelector toCategory: categoryName

"Moves the method aSelector (a String) from its current category to the
 specified category (also a String).  If either aSelector or categoryName is
 not in the receiver''s method dictionary, or if aSelector is already in
 categoryName, generates an error."

| theKeys   "Array of keys of dictionary categories"
  index     "loop index"
  oldCateg    "SymbolSet of selectors of original category"
  newCateg    "SymbolSet of selectors of new category"
  theSize   "size of theKeys"
  selectorSym 
|

"this method does not account for selectors inherited from superclasses"

self _validatePrivilege.
newCateg:= categories at: categoryName asSymbol .

theKeys:= (Array withAll: (categories keys)).
theSize := theKeys size.
selectorSym := aSelector asSymbol .
index:= 1.  "find original category of aSelector"

[((oldCateg:= (categories at: (theKeys at: index)))
                            includesValue: selectorSym )
  _or: [index >= theSize]
]
whileFalse:
   [index := index + 1].

(oldCateg includesValue: selectorSym ) 
  ifTrue: [ oldCateg remove: selectorSym ]
 ifFalse: [ ^ self _error: #classErrSelectorNotFound args: #[selectorSym] ].

(newCateg includesValue: selectorSym) "add aSelector"
ifTrue: [
  "selector already there, unlikely"
  self _uncontinuableError .
  ]
ifFalse: [newCateg add: selectorSym]
'.
			#'renameCategory:to:' -> 'renameCategory: categoryName to: newCategoryName

"Changes the name of the specified category to newCategoryName (a
 String), and returns the receiver.  If categoryName is not in the
 receiver''s method dictionary, or if newCategoryName is already in the
 receiver''s method dictionary, generates an error."

self _validatePrivilege.
(categories includesKey: (newCategoryName asSymbol))
ifTrue: [^ self _error: #classErrMethCatExists args: #[newCategoryName, self]].

(categories includesKey: (categoryName asSymbol))
ifTrue: [
   categories add: (SymbolAssociation newWithKey: (newCategoryName asSymbol)
                                      value: (categories at: categoryName)).
   categories removeKey: categoryName
   ]
ifFalse: [
   "key not found"
   ^ self _error: #classErrMethCatNotFound
            args: #[categoryName]
   ]
'.
			#'renameOrMergeCategory:to:' -> 'renameOrMergeCategory: oldName to: newName

"Changes the name of the specified category to newName (a String), and
 returns the receiver.  If oldName is not in the receiver''s method
 dictionary, generates an error.  If newName is already in the receiver''s
 category list, moves all the methods from the old category to the new
 category, and removes the old category."

| sels oldsym newsym |

self _validatePrivilege.
oldsym := oldName asSymbol.
sels := categories at: oldsym otherwise: nil.
sels == nil ifTrue: [
  ^ self _error: #classErrMethCatNotFound args: #[oldName] ].  "key not found"

newsym := newName asSymbol.
(categories includesKey: newsym) ifTrue: [
  sels size downTo: 1 do: [ :i |
    self moveMethod: (sels at: i) toCategory: newsym
  ].
  categories removeKey: oldsym ifAbsent: [].
]
ifFalse: [
  (categories includesKey: oldsym) ifTrue: [
    categories add:
	(SymbolAssociation new key: newsym value: (categories at: oldsym)).
    categories removeKey: oldsym.
  ].
].
'.
	}