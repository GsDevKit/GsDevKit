*monticello
copyMCMethodsFrom: sourceClass dictionaries: dicts
  "Copies all instance and class methods, pool dictionaries, and references
 to class variables from the given class to ourselves.  Returns an Array of
 methods in the source class which failed to compile.  Some of them
 might be class methods.  The Array will be empty if none failed."

  | failed srccls targcls cats clsComment |
  self _validatePrivilege.
  failed := Array new.
  (clsComment := sourceClass classComment) isEmpty not
    ifTrue: [ 
      "Copy class comment"
      self
        classComment: sourceClass classComment
        stamp: sourceClass commentStamp ].
  1 to: 2 do: [ :i | 
    "Copy class and instance methods"
    i == 1
      ifTrue: [ 
        srccls := sourceClass.
        targcls := self ]
      ifFalse: [ 
        srccls := sourceClass class.
        targcls := self class ].
    cats := srccls _categories.
    cats
      keysAndValuesDo: [ :cat :sels | 
        sels
          do: [ :sel | 
            | stamp |
            stamp := srccls stampForMethod: sel.
            [ 
            targcls compileMethod: (srccls sourceCodeAt: sel) category: cat using: dicts.
            targcls setStamp: stamp forMethod: sel ]
              onException: CompileError
              do: [ :ex | 
                | errorDetails def |
                errorDetails := (srccls compiledMethodAt: sel) class
                  _sourceWithErrors: ex errorDetails
                  fromString: (srccls sourceCodeAt: sel).
                def := MCMethodDefinition
                  className: self name
                  classIsMeta: i == 2
                  selector: sel
                  category: cat
                  timeStamp: stamp
                  source: (srccls sourceCodeAt: sel).
                MCFailedCopyMethodToNewClass signal: errorDetails with: def.
                failed
                  add:
                    {errorDetails.
                    def} ] ] ] ].
  ^ failed