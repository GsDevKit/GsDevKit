tests
testMonticelloAutoMigrateRefactor

	| instances list |
	Transcript cr; show: 'deployer---->testMonticelloAutoMigrateRefactor'.
	gofer version: 'GsDeployerBar-dkh.3'.
	gofer load.
	instances := OrderedCollection new.
	list := #( 
		GsDeployerBarA a:
		GsDeployerBarB b:
		GsDeployerBarC c:
		GsDeployerBarD d:
		).
	1 to: list size by: 2 do: [:index | | className selector instance |
		className := list at: index.
		selector := list at: index + 1.
		instance := (Smalltalk at: className) new.
		instances add: instance.
		instance perform: selector with: instance sampleValue ].
	instances do: [:instance |
		self assert: (instance perform: instance sampleValue) == (instance perform: #sampleValue) ].
	
	gofer version: 'GsDeployerBar-dkh.4'.
	GsDeployer deploy: [ gofer load ].
	instances do: [:instance |
		self assert: (instance perform: instance sampleValue) == (instance perform: #sampleValue).
		self assert: (instance perform: #e) == nil.
		self assert: (instance perform: #f) == nil ].
	Transcript cr; show: 'testMonticelloAutoMigrateRefactor<----deployer'.
