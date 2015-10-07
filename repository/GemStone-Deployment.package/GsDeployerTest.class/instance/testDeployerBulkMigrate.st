tests
testDeployerBulkMigrate
  | instances list deployer versionHistory originalClasses orphaned |
  gofer version: 'GsDeployerBar-dkh.1'.
  gofer load.
  instances := OrderedCollection new.
  originalClasses := IdentitySet new.
  list := #(#'GsDeployerBarA' #'a:' #'GsDeployerBarB' #'b:' #'GsDeployerBarC' #'c:' #'GsDeployerBarD' #'d:').
  1 to: list size by: 2 do: [ :index | 
    | className selector instance cls |
    className := list at: index.
    selector := list at: index + 1.
    cls := Smalltalk at: className.
    originalClasses add: cls.
    instance := cls new.
    instances add: instance.
    instance perform: selector with: instance sampleValue ].
  instances
    do: [ :instance | 
      self
        assert:
          (instance perform: instance sampleValue) == (instance perform: #'sampleValue') ].
  gofer version: 'GsDeployerBar-dkh.2'.
  deployer := GsDeployer new.
  (deployer objectNamed: #'UserGlobals')
    at: #'GsDeployerBar_Instances'
    put: instances.
  deployer commit.
  deployer bulkMigrate: true.
  deployer deploy: [ gofer load ].
  instances
    do: [ :instance | 
      self
        assert:
          (instance perform: instance sampleValue) == (instance perform: #'sampleValue').
      self assert: (instance perform: #'e') == nil ].
  versionHistory := MethodVersionHistory uniqueInstance dictionary.
  orphaned := false.
  originalClasses
    do: [ :cls | 
      "opportunistic test for https://github.com/GsDevKit/GsDevKit/issues/74"
      (cls classHistory includes: cls)
        ifFalse: [ 
          orphaned := true.
          versionHistory
            at: cls
            ifPresent: [ :ignored | 
              self
                assert: false
                description:
                  'Version history should be cleaned up during deploy:' ].
          versionHistory
            at: cls class
            ifPresent: [ :ignored | 
              self
                assert: false
                description:
                  'Version history should be cleaned up during deploy:' ] ] ].
  self assert: orphaned description: 'Expecting at least 1 class to be orphaned'