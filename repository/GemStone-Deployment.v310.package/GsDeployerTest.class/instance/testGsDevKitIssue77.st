tests
testGsDevKitIssue77
  "https://github.com/GsDevKit/GsDevKit/issues/77"

  "Simulate an upgrade situation where AClass[2] is in the new version 
   of GemStone and AClass[1] was in the old version of GemStone. After 
   upgrade of base system this is the class structure:

  Object
  Object
    GsDevKitBarA[1]
      GsDevKitBarC
    GsDevKitBarB
      GsDevKitBarA[2]

  During upgradeSeasideImage, the package containing the original 
  definition of CClass will come in. BUT ... MCClassDefinition 
  will short circuit the load and skip the creation of a new
  version of of CClass because the fact that AClass[2] is not ==
  to the superclass of CClass (i.e., AClass[1]), because only the
  name is checked."

  | repo classA classC superClassC |
  gofer version: 'GsDevKitBar-dkh.1'.
  gofer load.
  MCPlatformSupport
    suspendAutoCommitAndMigrationDuring: [ 
      | cl |
      "Create a new version of GsDevKitBarA under GsDevKitBarB"
      cl := (Smalltalk at: #'GsDevKitBarB')
        subclass: 'GsDevKitBarA'
        instVarNames: #()
        inDictionary: GsPackagePolicy current homeSymbolDict
        options: #().
      cl category: 'GsDevKitBar' ].
  false
    ifTrue: [ self halt	"browse class GsDevKitBar" ].
  gofer := (Smalltalk at: #'Gofer') new.
  gofer disablePackageCache.
  gofer version: 'GsDevKitBar-dkh.3'.
  repo := self monticelloRepository.
  gofer repository: repo.
  gofer load.
  false
    ifTrue: [ self halt	"browse class GsDevKitBar" ].
  self
    assert:
      (classA := Smalltalk at: #'GsDevKitBarA')
        == (superClassC := (classC := Smalltalk at: #'GsDevKitBarC') superclass)