doits
traitsMonticelloPreLoadDoIt
  Transcript
    cr;
    show: 'patching Monticello for load of trait support';
    cr.
  (GsSession currentSession symbolList objectNamed: 'MCClassDefinition')
    ifNotNil: [ :cls | 
      cls compile: 'traitCompositionString ^ ''{}''' classified: 'traits-compat'.
      cls
        compile: 'classTraitCompositionString ^ ''{}'''
        classified: 'traits-compat'.
      cls compile: 'sortedVariables ^ variables' classified: 'traits-compat' ]
