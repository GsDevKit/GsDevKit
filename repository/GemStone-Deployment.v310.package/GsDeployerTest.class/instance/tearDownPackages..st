running
tearDownPackages: aGoferInstance
  {'GsDeployerBar'.
  'GsDevKitBar'}
    do: [ :packageName | 
      (self hasPackage: packageName)
        ifTrue: [ aGoferInstance package: packageName ] ]