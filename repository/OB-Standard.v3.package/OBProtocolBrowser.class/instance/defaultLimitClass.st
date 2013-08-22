accessing
defaultLimitClass
    ^ (theClass isMeta ) 
        ifTrue: [Object class]
        ifFalse: [Object]