patch ops
equalDefinition: old to: new
  "address https://github.com/GsDevKit/GsDevKit/issues/77, where superclass of actual class is not the latest version of the superclass."

  (new isPerfectMatchToLoadedStateFor: old)
    ifFalse: [ 
      "treat as a modification"
      self modifyDefinition: old to: new ]