running
tearDownPackages: aGoferInstance

	{'GsDeployerBar' } do: [:packageName |
		(self hasPackage: packageName)
			ifTrue: [ aGoferInstance package: packageName ]].
