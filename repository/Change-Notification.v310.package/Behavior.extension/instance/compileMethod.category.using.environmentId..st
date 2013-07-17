*change-notification
compileMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId
  "Returns self, or signals a CompileError"
  | sel oldCat oldSource oldMeth oldStamp |
  sel := Behavior parseSelector: source for: self.
  (sel ~~ nil and: [(oldCat := self categoryOfSelector: sel) ~~ nil]) ifTrue: [ 
      oldMeth := self compiledMethodAt: sel.
      oldSource := self sourceCodeAt: sel.
      oldStamp := self stampForMethod: sel
  ].
  self _compileMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId.
  sel ifNotNil: [| meth |
      meth := self compiledMethodAt: sel.
      oldCat ifNotNil:[ 
          (oldCat asString ~= cat asString) ifTrue: [
              SystemChangeNotifier uniqueInstance 
                announce: (MethodMovedAnnouncement itemClass: self method: meth oldCategory: oldCat)
          ] ifFalse: [
              SystemChangeNotifier uniqueInstance 
                announce: (MethodModifiedAnnouncement itemClass: self method: meth oldCategory: oldCat)
          ]
        ] ifNil: [
          SystemChangeNotifier uniqueInstance 
              announce: (MethodAddedAnnouncement itemClass: self method: meth)
        ].
      MethodVersionHistory uniqueInstance addVersionFor: meth oldMeth: oldMeth oldStamp: oldStamp oldCategory: oldCat
  ].
  ^ self
