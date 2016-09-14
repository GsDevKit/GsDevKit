utilities
nextInt: anInteger from: aRandomGenerator
  "Answer a random integer in the interval [1, anInteger]."

  anInteger strictlyPositive
    ifFalse: [ self error: 'Range must be positive' ].
  ^ (aRandomGenerator next * anInteger) truncated + 1
