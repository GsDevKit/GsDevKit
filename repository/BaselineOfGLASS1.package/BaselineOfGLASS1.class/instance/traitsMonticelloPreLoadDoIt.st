doits
traitsMonticelloPreLoadDoIt

  Transcript cr; show: 'patching Monticello for load of trait support'; cr.
  (Smalltalk classNamed: 'MCClassDefinition') ifNotNil: [:cls |
    cls compile: 'traitCompositionString ^ ''{}''' classified: 'accessing'.
    cls compile: 'classTraitCompositionString ^ ''{}''' classified: 'accessing'. ]