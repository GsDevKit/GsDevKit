*bootstrap
baseOverridesBootstrapSessionMethodBehavior: known
	"Behavior methods that get overridden when session methods are enabled"

	self
		addTo: known
		forBehavior: Behavior
		overrides: {
			#'selectorsIn:' -> 'selectorsIn: categoryName

"Returns an Array of all selectors in the specified category.  If categoryName
 is not in the receiver''s method dictionary, generates an error."

^ Array withAll: (categories at: (categoryName asSymbol)) 
'.
			#'categoryNames' -> 'categoryNames

"Returns an Array of Symbols.  The elements of the Array are the
 receiver''s category names (excluding names inherited from superclasses)."

 | result j |
 result := Array new: categories size .
 j := 1 .
 categories keysAndValuesDo:[ :aKey :aValue |
   result at: j put: aKey .
   j := j + 1 
   ]. 
 ^ result
'.
			#'removeSelector:ifAbsent:' -> 'removeSelector: aString ifAbsent: aBlock

"Removes the method whose selector is aString from the receiver''s
 method dictionary.  If the selector is not in the method
 dictionary, returns the result of evaluating the
 zero-argument block aBlock.  Otherwise, returns the receiver."

| aKey |

self _validatePrivilege.
aKey := aString asSymbol .
(methodDict includesKey: aKey) ifFalse:[ ^ aBlock value ] .
self removeSelector: aKey .
'.
			#'_methodDict' -> '_methodDict

"Returns the method dictionary."

^ methodDict
'.
			#'removeAllMethods' -> 'removeAllMethods

"Removes all methods from the receiver.  This should not be done without
 considerable forethought!"

self _validatePrivilege.
categories ~~ nil ifTrue: [ categories removeAll ].
methodDict ~~ nil ifTrue: [ methodDict removeAll ].
self _refreshClassCache.
'.
			#'sortedCategoryNames' -> 'sortedCategoryNames

"Returns an Array of Symbols.  The elements of the collection are the
 receiver''s category names (excluding names inherited from superclasses)."

 | coll |
 coll := SortedCollection new .
 categories keysAndValuesDo:[:aKey :aValue | coll add: aKey ].
 ^ Array withAll: coll .
'.
			#'includesSelector:' -> 'includesSelector: aString

"Returns true if the receiver defines a method for responding to aString."

| aSym |
aSym := Symbol _existingWithAll: aString.
aSym == nil ifTrue:[ ^ false ].
^ methodDict includesKey: aSym 
'.
			#'compileMethod:dictionaries:category:' -> 'compileMethod: sourceString
dictionaries: aSymbolList
category: aCategoryString

"This compiles some source code for the receiver.  The first argument,
 sourceString, is the string of source code to be compiled.  The second
 argument is a SymbolList to be used in parsing, along with the list of all
 class variables and pool dictionaries for the receiver and all of its
 superclasses.  The third argument (a String) indicates the method''s category.

 sourceString must be a kind of String or DoubleByteString.  Instances of
 JapaneseString are not supported as source strings.  String literals
 (''abc'') are generated as instances of the class of sourceString, 
 unless sourceString is a Symbol, in which case ''abc'' produces a String.
 If sourceString is a DoubleByteSymbol, ''abc'' produces a DoubleByteString.

 If there are no errors, this adds the resulting compiled method to the
 receiver''s method dictionary and returns nil.

 If errors occur, the result is an Array of error descriptors which can be
 be used as an input to the GsMethod (C) | _sourceWithErrors:fromString: 
 method.
 
 An error descriptor is an Array of size 3 or 4, containing the following
 elements:

 1. The GemStone error number.
 2. Offset into the source string where the error occurred.
 3. Error message text, if available, or nil.
 4. Internal compiler error text, if the error is internal."

| symList categ result |

self _validatePrivilege.
aSymbolList class == SymbolList
  ifTrue:[ symList := aSymbolList ]
  ifFalse:[
    aSymbolList _validateClass: Array .
    symList := SymbolList withAll: aSymbolList .
    ] .
categ := aCategoryString asSymbol .

Segment setCurrent: self segment while:[
  result := self compileMethod: sourceString
       dictionaries: symList
       category: categ  intoMethodDict: nil intoCategories: nil
       intoPragmas: nil .

  result _class == GsMethod ifTrue:[ result := nil ].
].
^ result
'.
			#'removeSelector:' -> 'removeSelector: aString

"Removes the method whose selector is aString from the receiver''s method
 dictionary.  If the selector is not in the method dictionary, generates an
 error.  Any breakpoints in the removed method are cleared.
"

| selector |

self _validatePrivilege.
self _basicRemoveSelector: (selector := aString asSymbol) .
categories associationsDo: [:anAssoc | | setOfSelectors |
  setOfSelectors:= anAssoc value.
  (setOfSelectors remove: selector ifAbsent:[ nil ]) ~~ nil ifTrue:[ 
    ^ self "done"
    ]
  ].

"if we get here , we could not find the selector in any category "
^ self _error: #classErrSelectorNotFound args: #[aString] .
'.
			#'_removeAllSubclassCode' -> '_removeAllSubclassCode

"Dereference the code objects of all GsMethods of the receiver,
 to force recompilation of those methods."

| theSubclasses |
self _validatePrivilege.
theSubclasses := self _subclasses .
theSubclasses ~~ nil ifTrue:[
  theSubclasses do:[:x| x _removeAllSubclassCode ] .
  ].
methodDict keysAndValuesDo: [ :aKey :aMethod |
  methodDict at: aKey put: ((methodDict at: aKey) _copyToForceRecompilation).
  ].
self _refreshClassCache .
^ self
'.
			#'removeCategory:' -> 'removeCategory: categoryName

"Removes the specified category and all its methods from the receiver''s
 method dictionary.  If categoryName is not in the receiver''s method
 dictionary, generates an error.
 Any breakpoints in removed methods are cleared."

| theSelectors |

self _validatePrivilege.
theSelectors := categories at: (categoryName asSymbol).
1 to: theSelectors size do: [ :each |
  self _basicRemoveSelector: (theSelectors at: each)
  ].
categories removeKey: (categoryName asSymbol)
           ifAbsent: [ ^ self _error: #classErrMethCatNotFound
                              args: #[categoryName]]
'.
			#'selectors' -> 'selectors

"Returns an Array of Symbols, consisting of all of the message selectors
 defined by the receiver.  (Selectors inherited from superclasses are not
 included.)  For keyword messages, the Symbol includes each of the keywords,
 concatenated together."

^ Array withAll: (methodDict keys)
'.
			#'sourceCodeAt:' -> 'sourceCodeAt: aSelector

"Returns a String representing the source code for the argument, aSelector.  If
 aSelector (a String) is not a selector in the receiver''s method dictionary,
 this generates an error."

|method aSym|

aSym := Symbol _existingWithAll: aSelector.
aSym == nil ifTrue:[ ^ methodDict _errorKeyNotFound: aSelector ].
((method:= methodDict at: aSym ) ~~ nil)
ifTrue:
   [^ method _sourceString]
ifFalse:
   [^ nil]
'.
			#'_addCategory:' -> '_addCategory: categoryName

"Returns the Array representing the new category."

self _validatePrivilege.
self addCategory: categoryName .
^ categories at: categoryName asSymbol
'.
			#'_categories' -> '_categories

"Returns the GsMethodDictionary containing this instance''s category
 organization."

^ categories
'.
			#'_categoriesReport' -> '_categoriesReport

"Returns an Array containing key-value pairs from the receiver''s categories.
 The key in each key-value pair is the name of a category; the value in each
 key-value pair is a sorted Array of selectors."

"Used by the Topaz ''list categories'' command."

| result assocs anAssoc k |
assocs := IdentitySet new .
categories keysAndValuesDo:[:categName :selectors| | sortedSelectors |
  sortedSelectors := SortedCollection new .
  selectors do:[:aSelector| sortedSelectors add: aSelector ].
  sortedSelectors := Array withAll: sortedSelectors .
  assocs add: (SymbolAssociation newWithKey: categName value: sortedSelectors) 
  ].
assocs := assocs sortAscending: ''key'' .
result := Array new: assocs size * 2  .
k := 1 .
1 to: assocs size do:[:j | 
 anAssoc := assocs at: j . 
 result at: k put: anAssoc key .
 result at: k + 1 put: anAssoc value .
 k := k + 2 .
 ].
^ result .
'.
			#'_selectorWithSource:' -> '_selectorWithSource: aString

"Returns the selector for the method whose source string is identical to
 aString."

methodDict keysAndValuesDo:[:aSelector :aMethod |
  (aMethod _sourceString == aString) ifTrue: [^ aSelector ]
  ].
^nil
'.
			#'compiledMethodAt:' -> 'compiledMethodAt: aSelector

"Returns the compiled method associated with the argument aSelector (a String).
 The argument must be a selector in the receiver''s method dictionary; if it is
 not, this method generates an error."

| aSym |
aSym := Symbol _existingWithAll: aSelector.
aSym == nil ifTrue:[ ^ methodDict _errorKeyNotFound: aSelector ].
^ methodDict at: aSym
'.
			#'compiledMethodAt:' -> 'compiledMethodAt: aSelector

"Returns the compiled method associated with the argument aSelector (a String).
 The argument must be a selector in the receiver''s method dictionary; if it is
 not, this method generates an error."

| aSym |
aSym := Symbol _existingWithAll: aSelector.
aSym == nil ifTrue:[ ^ methodDict _errorKeyNotFound: aSelector ].
^ methodDict at: aSym
'.
			#'categoryOfSelector:' -> 'categoryOfSelector: aSelector

"Returns a Symbol which is the name of the category for the specified
 selector, or nil if the selector was not found in any category."

| aSymbol |
aSymbol := Symbol _existingWithAll: aSelector .
aSymbol == nil ifTrue:[ ^ nil ].
categories keysAndValuesDo: [:aKey :aValue | 
  (aValue includesIdentical: aSymbol ) ifTrue:[ ^ aKey ].
  ].
^ nil
'.
			#'sortedSelectorsIn:' -> 'sortedSelectorsIn: categoryName

"Returns an Array of all selectors in the specified category, sorted
 in ascending order."

 | coll |
 coll := SortedCollection new .
 (categories at: (categoryName asSymbol)) do:[:aKey | coll add: aKey ].
 ^ Array withAll: coll
'.
			#'_includesCategory:' -> '_includesCategory: aString

"Returns true if aString is equivalent to the name of a category in the
 receiver, false otherwise."

^ categories includesKey: aString asSymbol
'.
			#'pragmasForMethod:' -> 'pragmasForMethod: selector
        "Need to install SessionMethod support to access Pragmas"

        ^#()
'.
			#'theNonMetaClass' -> 'theNonMetaClass
  "Sent to a class or metaclass, always return the class.
   Used by topaz and Monticello"

  ^self
'.
			#'whichClassIncludesSelector:' -> 'whichClassIncludesSelector: aString

"If the selector aString is in the receiver''s method dictionary,
 returns the receiver.  Otherwise, returns the most immediate superclass
 of the receiver where aString is found as a message selector.  Returns
 nil if the selector is not in the method dictionary of the receiver or
 any of its superclasses."

  | currClass aSymbol |
  aSymbol := Symbol _existingWithAll: aString .
  aSymbol == nil ifTrue:[ ^ nil ].

  "Check the immediate class directly"
  (methodDict includesKey: aSymbol) ifTrue: [^self].

  "Okay, check the superclasses"
  currClass := superClass.

  [ currClass == nil ifTrue: [^nil].
    currClass _methodDict includesKey: aSymbol]
  whileFalse:
    [ currClass := currClass superClass ].

  ^currClass
'.
	}