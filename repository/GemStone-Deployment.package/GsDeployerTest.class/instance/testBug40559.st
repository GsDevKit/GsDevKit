tests
testBug40559

	| instances list instance className |
	Transcript cr; show: 'deployer---->testBug40559'.
	[gofer version: 'GsDeployerBar-dkh.1'.
	gofer load.
	instances := OrderedCollection new.
	list := #( 
		a
		CV
		civ
		).
	className := #GsDeployerBarA.
	instance := (Smalltalk at: className) new.
	instances add: instance.
	list do: [:selector |
		instance perform: selector asMutator with: instance sampleValue ].
	instances do: [:instance |
		list do: [:selector |
			self assert: (instance perform: selector) == (instance perform: #sampleValue) ]].
	gofer version: 'GsDeployerBar-dkh.2'.
	GsDeployer deploy: [ gofer load ].
	instances do: [:instance |
		list do: [:selector |
			self assert: (instance perform: selector) == (instance perform: #sampleValue) ]]] on: Error do: [:ex | Transcript cr; show: ex description. self halt ].
	Transcript cr; show: 'testBug40559<----deployer'.
