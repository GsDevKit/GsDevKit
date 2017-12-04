private
collectGlobalsAndExtensionMethodsDuring: aBlock
  GsPackagePolicy enabled
    ifTrue: [
 	| home external deploymentDict |
	home := GsPackagePolicy current homeSymbolDict.
	external := GsPackagePolicy current externalSymbolList.
	deploymentDict := SymbolDictionary new.
	deploymentDict at: #DEPLOYER put: deploymentDict.
	[
		GsPackagePolicy current homeSymbolDict: deploymentDict.
		GsPackagePolicy current externalSymbolList: GsSession currentSession symbolList asArray.
		aBlock value ]
			ensure:  [ 
				GsPackagePolicy current homeSymbolDict: home.
				GsPackagePolicy current externalSymbolList: external ].
	^deploymentDict ]
    ifFalse: [
      "if GsPackagePolicy is not enabled, assume we are running as SystemUser"
      aBlock value.
      ^ Globals ]
