private
collectGlobalsAndExtensionMethodsDuring: aBlock
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
	^deploymentDict