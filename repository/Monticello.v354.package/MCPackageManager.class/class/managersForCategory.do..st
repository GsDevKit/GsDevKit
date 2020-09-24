private
managersForCategory: aSystemCategory do: aBlock
  "if you can find a manager, then go ahead and evaluate aBlock. Don't create a manager if it doesn't already exist!"

  | cat index found |
  cat := aSystemCategory ifNil: [ ^ nil ].
  found := false.
  self registry
    do: [ :mgr | 
      "first ask PackageInfos, their package name might not match the category, 
		aSystemCategory may be extracted from a method protocol"
      ((mgr packageInfo includesSystemCategory: aSystemCategory)
        or: [ mgr packageInfo isYourClassExtension: aSystemCategory ])
        ifTrue: [ 
          aBlock value: mgr.
          found := true ] ].
  found
    ifTrue: [ ^ self ].
  [ 
  "Loop over categories until we found a matching one"
  self registry
    at: (MCPackage named: cat)
    ifPresent: [ :mgr | 
      aBlock value: mgr.
      ^ self ].
  index := cat lastIndexOf: $-.
  index > 0 ]
    whileTrue: [ 
      "Step up to next level package"
      cat := cat copyFrom: 1 to: index - 1 ]