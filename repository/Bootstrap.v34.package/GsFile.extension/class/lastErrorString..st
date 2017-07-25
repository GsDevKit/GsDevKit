*bootstrap
lastErrorString: onClient
  ^ onClient
    ifTrue: [ self lastErrorString ]
    ifFalse: [ self serverErrorString ]