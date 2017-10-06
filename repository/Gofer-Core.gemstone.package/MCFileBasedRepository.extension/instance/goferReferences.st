*gofer-core-accessing
goferReferences
  | versionNames |
  versionNames := [ self allVersionNames ]
    on: Error
    do: [ :error | ^ GoferRepositoryError signal: error description repository: self ].
  ^ (versionNames reject: [ :each | each isEmpty ])
    collect: [ :each | GoferResolvedReference name: each repository: self ]