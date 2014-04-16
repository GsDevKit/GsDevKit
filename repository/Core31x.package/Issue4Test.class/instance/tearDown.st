Running
tearDown

  super tearDown.
  Smalltalk at: self issue4ClassName ifPresent: [:cl | cl removeFromSystem ].
  (SystemOrganizer new categories includes: self issue4CategoryName)
    ifTrue: [ SystemOrganizer new removeCategory: self issue4CategoryName ].