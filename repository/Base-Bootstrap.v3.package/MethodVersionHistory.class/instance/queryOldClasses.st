private
queryOldClasses
  "return all keys which are obsolete (e.g. old classes from the class history)"

  | symbolList |
  symbolList := GsSession currentSession symbolList.
  ^ dictionary keys
    select: [ :cl | 
      cl isMeta
        ifTrue: [ 
          (symbolList objectNamed: cl thisClass name) notNil
            and: [ cl ~~ (symbolList objectNamed: cl thisClass name) class ] ]
        ifFalse: [ 
          (symbolList objectNamed: cl name) notNil
            and: [ cl ~~ (symbolList objectNamed: cl name) ] ] ]