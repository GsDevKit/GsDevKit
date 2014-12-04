tests
testOverriddenBaseMethods
  | overrides knownOverrides |
  true
    ifTrue: [ 
      "no longer necessary to check"
      ^ self ].
  overrides := IdentityDictionary new.
  knownOverrides := self knownOverrides.
  Globals
    valuesDo: [ :behavior | 
      behavior isBehavior
        ifTrue: [ 
          self
            findOverridenMethodsFor: behavior theMetaClass
            overrides: overrides
            knownOverrides: knownOverrides.
          self
            findOverridenMethodsFor: behavior theNonMetaClass
            overrides: overrides
            knownOverrides: knownOverrides ] ].
  self assert: overrides isEmpty