filtering
defaultLimitClass
    "it's too bad that this needs to be duplicated between the ProtocolBrowser and here'"
    ^ (theClass isMeta )
        ifTrue: [Object class]
        ifFalse: [Object]
