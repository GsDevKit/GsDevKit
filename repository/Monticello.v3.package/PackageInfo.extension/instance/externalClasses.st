*monticello
externalClasses
  | myClasses |
  myClasses := self classesAndMetaClasses.
  ^ Array
    streamContents: [ :s | 
      MCPlatformSupport
        allBehaviorsDo: [ :class | 
          (myClasses includes: class)
            ifFalse: [ s nextPut: class ] ] ]