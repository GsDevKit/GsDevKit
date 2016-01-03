running
tearDownPackages: aGoferInstance
  {'GsDeployerBar'.
  'GsDevKitBar'.
  'GsDevKit83Foo'.
  'GsDevKit83Bar'}
    do: [ :packageName | 
      (self hasPackage: packageName)
        ifTrue: [ aGoferInstance package: packageName ] ]