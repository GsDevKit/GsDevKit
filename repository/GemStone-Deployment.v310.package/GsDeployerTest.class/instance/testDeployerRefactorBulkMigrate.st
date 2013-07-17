tests
testDeployerRefactorBulkMigrate

	| instances list deployer exceptions |
	gofer version: 'GsDeployerBar-dkh.3'.
	gofer load.
	instances := OrderedCollection new.
	list := #( 
		GsDeployerBarA a:
		GsDeployerBarB b:
		GsDeployerBarC c:
		GsDeployerBarD d:
		).
	exceptions := IdentitySet new.
	1 to: list size by: 2 do: [:index | | className selector instance |
		className := list at: index.
		selector := list at: index + 1.
		instance := (Smalltalk at: className) new.
		instances add: instance.
		instance perform: selector with: instance sampleValue.
		(instance respondsTo: #f:)
			ifTrue: [ 
				instance perform: #f: with: instance sampleValue.
				exceptions add: instance ] ].
	instances do: [:instance |
		self assert: (instance perform: instance sampleValue) == (instance perform: #sampleValue) ].
	
	gofer version: 'GsDeployerBar-dkh.4'.
	deployer := GsDeployer new.
	(deployer objectNamed: #UserGlobals) at: #'GsDeployerBar_Instances' put: instances.
	deployer commit.
	deployer bulkMigrate: true.
	deployer deploy: [ gofer load ].
	instances do: [:instance |
		self assert: (instance perform: instance sampleValue) == (instance perform: #sampleValue).
		self assert: (instance perform: #e) == nil.
		(exceptions includes: instance)
			ifTrue: [ self assert: (instance perform: #f) == (instance perform: #sampleValue) ]
			ifFalse: [ self assert: (instance perform: #f) == nil ] 
	].
