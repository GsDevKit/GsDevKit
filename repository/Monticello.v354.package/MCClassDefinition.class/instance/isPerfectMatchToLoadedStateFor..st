testing
isPerfectMatchToLoadedStateFor: loadedDefinition
  "loadedDefinition should match the state of the image. 
   Answer true if the receiver is a perfect match to the image."

  ^ (super isPerfectMatchToLoadedStateFor: loadedDefinition)
    and: [ 
      "if superclass of actualClass is different than the superClass 
       found in Smalltalk, we are not a perfect match - implication is
       that the receiver should be loaded."
      self actualClass superclass == (Smalltalk at: superclassName asSymbol) ]