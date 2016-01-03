running
tearDownPackages: aGoferInstance
  {'GsDeployerBar'.
  'GsDevKitBar'.
  'GsDevKit83Foo'.
  'GsDevKit83Bar'.
  'GsDevKit83Baz'}
    do: [ :packageName | 
      (self hasPackage: packageName)
        ifTrue: [ aGoferInstance package: packageName ] ]