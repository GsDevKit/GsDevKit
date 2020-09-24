*bootstrap
baseOverridesBootstrapTestCase: known

	self 
		addTo: known 
		forBehavior: TestCase 
		overrides: {
			#executeShould:inScopeOf: -> 'executeShould: aBlock inScopeOf: anExceptionalEvent 
	^[aBlock value.
 	false] sunitOn: anExceptionalEvent
		do: [:ex | ex sunitExitWith: true]
'.
			#should:raise: -> 'should: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
'.
			}