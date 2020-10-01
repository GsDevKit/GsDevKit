accessing
dictionary
  dictionary == nil
    ifTrue: [ dictionary := IdentityKeyValueDictionary new ].
  ^ dictionary