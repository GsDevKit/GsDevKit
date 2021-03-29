*bootstrap
baseOverridesBootstrapObject: known
	self
		addTo: known
		forBehavior: Object
		overrides: {
			"2.4.4"
			#'migrateIndexable:myClass:otherClass:' -> 'migrateIndexable: anotherObject myClass: cls otherClass: othercls

"Handle migrating the indexable component of an object."

| validate instSize cons oldVal value |

validate := [ :val :cls | (val isKindOf: cls) ifTrue: [val] ifFalse: [nil] ].
instSize := cls instSize.
(cls isIndexable _and: [ othercls isIndexable])
  ifTrue:[ " both classes are indexable "
    cls isBytes
      ifTrue:[
        ( (cls isKindOf: CharacterCollection) _and:
        [ othercls isKindOf: CharacterCollection ] )
          ifTrue:[ " both are CharacterCollections "
            anotherObject copyFrom:1 to: anotherObject size into: self startingAt: 1.
          ]
         ifFalse:[
           instSize := cls instSize.
           self _basicSize: anotherObject _basicSize.
           1 to: anotherObject _basicSize do: [:j |
             self _unsafeAt: j+instSize put: (anotherObject _basicAt: j). 
           ].
         ].
      ]
      ifFalse: [
        instSize := cls instSize.
        cons := cls varyingConstraint.
        self _basicSize: anotherObject _basicSize.
        1 to: anotherObject _basicSize do: [:j |
          oldVal := anotherObject _at: j.
          value := validate value: oldVal value: cons.
          " see if old value was changed to nil due to varying constraint "
          ( oldVal ~~ nil _and: [ value == nil ] )
            ifTrue: [
              " raise error or possibly get new value "
              value := anotherObject
                invalidElementConstraintWhenMigratingInto: self
                for: oldVal.

              " if value was converted, insert it "
              value ~~ oldVal
                ifTrue: [ self _unsafeAt: j+instSize put: value ].
            ]
            ifFalse: [ self _unsafeAt: j+instSize put: value ]
        ].
      ].
  ].
'.
			"2.3.x"
			#'migrateIndexable:myClass:otherClass:' -> 'migrateIndexable: anotherObject myClass: cls otherClass: othercls

"Handle migrating the indexable component of an object."

| validate instSize cons oldVal value |

validate := [ :val :cls | (val isKindOf: cls) ifTrue: [val] ifFalse: [nil] ].
instSize := cls instSize.
(cls isIndexable _and: [ othercls isIndexable])
  ifTrue:[ " both classes are indexable "
    cls isBytes
      ifTrue:[
        ( (cls isKindOf: CharacterCollection) _and:
        [ othercls isKindOf: CharacterCollection ] )
          ifTrue:[ " both are CharacterCollections "
            anotherObject copyFrom:1 to: anotherObject size into: self startingAt: 1.
          ]
         ifFalse:[
           instSize := cls instSize.
           self _basicSize: anotherObject _basicSize.
           1 to: anotherObject _basicSize do: [:j |
             self _unsafeAt: j+instSize put: (anotherObject _at: j) asciiValue 
           ].
         ].
      ]
      ifFalse: [
        instSize := cls instSize.
        cons := cls varyingConstraint.
        self _basicSize: anotherObject _basicSize.
        1 to: anotherObject _basicSize do: [:j |
          oldVal := anotherObject _at: j.
          value := validate value: oldVal value: cons.
          " see if old value was changed to nil due to varying constraint "
          ( oldVal ~~ nil _and: [ value == nil ] )
            ifTrue: [
              " raise error or possibly get new value "
              value := anotherObject
                invalidElementConstraintWhenMigratingInto: self
                for: oldVal.

              " if value was converted, insert it "
              value ~~ oldVal
                ifTrue: [ self _unsafeAt: j+instSize put: value ].
            ]
            ifFalse: [ self _unsafeAt: j+instSize put: value ]
        ].
      ].
  ].
'.	}