*gssqueakcommon-corev30
primesUpTo: max do: aBlock
  "Compute aBlock with all prime integers up to the given integer."

  "Integer primesUpTo: 100"

  | index limit limitSqrtFloor sieve increment |
  limit := max asInteger.
  limit <= 1
    ifTrue: [ ^ self ].	"Fall back into #largePrimesUpTo:do: if we'd require more than 100k of memory; 
	the alternative will only requre 1/154th of the amount we need here and is almost as fast."
  limit > 25000
    ifTrue: [ ^ self largePrimesUpTo: limit do: aBlock ].
  limit := limit - 1.	"upTo:"
  sieve := Array new: limit withAll: true.
  sieve at: 1 put: false.
  index := 2.
  limitSqrtFloor := limit sqrtFloor.
  increment := 1.
  [ index <= limitSqrtFloor ]
    whileTrue: [ 
      (sieve at: index)
        ifTrue: [ 
          | notPrimeIndex notPrimeIncrement |
          aBlock value: index.
          notPrimeIndex := index * index.
          notPrimeIncrement := increment * index.
          [ notPrimeIndex <= limit ]
            whileTrue: [ 
              sieve at: notPrimeIndex put: false.
              notPrimeIndex := notPrimeIndex + notPrimeIncrement ] ].
      index := index + increment.
      increment := 2 ].
  [ index <= limit ]
    whileTrue: [ 
      (sieve at: index)
        ifTrue: [ aBlock value: index ].
      index := index + increment.
      increment := 2 ]