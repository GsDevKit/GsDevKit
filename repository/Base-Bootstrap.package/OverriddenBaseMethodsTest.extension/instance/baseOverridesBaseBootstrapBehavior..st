*base-bootstrap
baseOverridesBaseBootstrapBehavior: known
	self
		addTo: known
		forBehavior: Behavior
		overrides: {
			#'classVarNames' -> 'classVarNames

"Returns an Array of Symbols naming the class variables defined by this class.
 Inherited class variables are not included; contrast with allClassVarNames."

| result |

result:= Array new.
(classVars == nil)
ifTrue:
   [^ result]
ifFalse:
[
   (classVars keys) do: "get them from the dictionary"
       [:each| result add: each].
    ^ result
]
'.
			#'recompileAllMethodsInContext:' -> 'recompileAllMethodsInContext: aSymbolList

"Recompiles all methods for the receiver, using the specified symbol list.

 This method is designed to allow a user interface to issue GciContinue after
 fixing the source code for a method in error.  GciContinue will reattempt the
 compilation of the method which contained an error, then proceed to the next
 method."

| categoryList |
self _validatePrivilege.
categoryList := categories copy .
categoryList associationsDo: [:aCategoryAssoc| | selectorList theCategory | 
  theCategory := aCategoryAssoc key .
  selectorList := aCategoryAssoc value copy .
  selectorList do: [ :aSelector| | theSource  errors |
    [ "this Block supports GciContinue"
      "Make sure the method is there before trying to recompile it.
       An exception handler may have removed the method!"
      (self includesSelector: aSelector) ifTrue: [
        theSource:= self sourceCodeAt: aSelector .
        errors := self compileMethod: theSource dictionaries: aSymbolList
                        category: theCategory .
        (errors == nil) ifFalse:[
          System signal: (ErrorSymbols at: #compilerErrStDB)
            args:#[errors, theSource, self, theCategory, aSymbolList, aSelector]
            signalDictionary: GemStoneError .
        ].
      ]
      ifFalse: [
        errors := nil
      ].
      errors == nil
    ] untilTrue
  ] "end do selectors"
].  "end do categories"
^ self
'.
	}