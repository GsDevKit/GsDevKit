tests
testMonticelloAutoMigrate
  | instances list |
  Transcript
    cr;
    show: 'deployer---->testMonticelloAutoMigrate'.
  [ 
  gofer version: 'GsDeployerBar-dkh.1'.
  gofer load.
  instances := OrderedCollection new.
  list := #(#'GsDeployerBarA' #'a:' #'GsDeployerBarB' #'b:' #'GsDeployerBarC' #'c:' #'GsDeployerBarD' #'d:').
  1 to: list size by: 2 do: [ :index | 
    | className selector instance |
    className := list at: index.
    selector := list at: index + 1.
    instance := (Smalltalk at: className) new.
    instances add: instance.
    instance perform: selector with: instance sampleValue ].
  instances
    do: [ :instance | 
      self
        assert:
          (instance perform: instance sampleValue) == (instance perform: #'sampleValue') ].
  gofer version: 'GsDeployerBar-dkh.2'.
  GsDeployer deploy: [ gofer load ].
  instances
    do: [ :instance | 
      self
        assert:
          (instance perform: instance sampleValue) == (instance perform: #'sampleValue').
      self assert: (instance perform: #'e') == nil ] ]
    on: Error
    do: [ :ex | 
      Transcript
        cr;
        show: ex description;
        show: (GsProcess stackReportToLevel: 100).
      ex pass ].
  Transcript
    cr;
    show: 'testMonticelloAutoMigrate<----deployer'