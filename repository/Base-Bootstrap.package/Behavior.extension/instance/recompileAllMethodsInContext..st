*base-bootstrap
recompileAllMethodsInContext: aSymbolList

"Recompiles all methods for the receiver, using the specified symbol list.

 This method is designed to allow a user interface to issue GciContinue after
 fixing the source code for a method in error.  GciContinue will reattempt the
 compilation of the method which contained an error, then proceed to the next
 method."

| categoryList |
self _validatePrivilege.
categoryList := self _categories .
categoryList associationsDo: [:aCategoryAssoc| | selectorList theCategory | 
  theCategory := aCategoryAssoc key .
  selectorList := aCategoryAssoc value copy .
  selectorList do: [ :aSelector| | theSource  stamp errors |
    [ "this Block supports GciContinue"
      "Make sure the method is there before trying to recompile it.
       An exception handler may have removed the method!"
      (self includesSelector: aSelector) ifTrue: [
        theSource:= self sourceCodeAt: aSelector .
 	  stamp := self stampForMethod: aSelector.
       errors := self compileMethod: theSource 
                        category: theCategory 
                        using: aSymbolList.
        (errors == nil) ifFalse:[
          System signal: (ErrorSymbols at: #compilerErrStDB)
            args:#[errors, theSource, self, theCategory, aSymbolList, aSelector]
            signalDictionary: GemStoneError .
        ].
        self setStamp: stamp forMethod: aSelector.
      ]
      ifFalse: [
        errors := nil
      ].
      errors == nil
    ] untilTrue
  ] "end do selectors"
].  "end do categories"
^ self