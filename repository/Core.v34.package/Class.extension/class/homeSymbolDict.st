*core-squeak-subclass creation
homeSymbolDict
  "if GsPackagePolicy is enabled, use it's homeSymbolDict ... otherwise use Globals -- we must be SystemUser"

  ^ GsPackagePolicy enabled
      ifTrue: [ GsPackagePolicy current homeSymbolDict ]
      ifFalse: [ Globals ] 
