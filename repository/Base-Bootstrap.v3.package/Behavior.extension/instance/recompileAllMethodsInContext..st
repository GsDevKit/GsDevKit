*base-bootstrap
recompileAllMethodsInContext: aSymbolList

"Recompiles all methods for the receiver, using the specified symbol list.

 This method is designed to allow a user interface to issue GciContinue after
 fixing the source code for a method in error.  GciContinue will reattempt the
 compilation of the method which contained an error, then proceed to the next
 method."

| envId |
self _validatePrivilege.
envId := 0 .
self env: envId categorysDo:[  :categName :selectorList |
  selectorList copy do: [ :aSelector| | aMeth err |
    [ "this Block supports GciContinue"
      "Make sure the method is there before trying to recompile it.
       An exception handler may have removed the method!"
      err := nil .
      aMeth := self compiledMethodAt: aSelector otherwise: nil .
      aMeth ifNotNil:[ | theSource methEnvId |
        theSource:= aMeth sourceString  .
        methEnvId := aMeth environmentId .
        methEnvId == envId ifFalse:[ self error:'environmentId mismatch'].
        [ | stamp |
          stamp := self stampForMethod: aSelector.
          self compileMethod: theSource 
                        category: categName 
                        using: aSymbolList environmentId: envId .
          self setStamp: stamp forMethod: aSelector.
        ] onException: CompileError do:[:ex |
          err := ex .
          ex outer .
        ]
      ].
      err == nil
    ] untilTrue
  ].
].
^ self
