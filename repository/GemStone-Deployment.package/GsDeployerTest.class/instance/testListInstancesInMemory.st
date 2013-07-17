tests
testListInstancesInMemory

	| instances list classes instancesList |
	gofer version: 'GsDeployerBar-dkh.1'.
	gofer load.
	instances := OrderedCollection new.
	list := #( 
		GsDeployerBarA a:
		GsDeployerBarB b:
		GsDeployerBarC c:
		GsDeployerBarD d:
		).
	classes := OrderedCollection new.
	1 to: list size by: 2 do: [:index | | className selector instance |
		className := list at: index.
		selector := list at: index + 1.
		instance := (Smalltalk at: className) new.
		instances add: instance.
		instance perform: selector with: instance sampleValue.
		classes add: instance class ].
	instancesList :=  ((System stoneVersionAt: 'gsVersion') beginsWith: '3.')
		ifTrue: [ (SystemRepository perform: #listInstancesInMemory: with: classes asArray) ]
		ifFalse: [ 
			System commitTransaction ifFalse: [self error: 'Commit failed!'].
			SystemRepository listInstances: classes asArray ].
	1 to: instancesList size do: [:i | | insts class instance |
		class := (classes at: i).
		instance := (instances at: i).
		self assert: instance class == class.
		insts := instancesList at: i.
		insts do: [:inst | 
			self assert: inst class == class.
			self assert: (inst == instance) ].
		self assert: insts size = 1 ].
