tests
expectedFailures

	^#(
		testCannotLoadClass "this test collides somehow with MCGsClassModificationTestCase
		a proper fix would be 
		MCClassDefinition>>createClass
		
		... 
		        (traitComposition ~= '{}' or: [classTraitComposition ~= '{}']) ifTrue: [
      			ImproperOperation new reason: #rtErrTraitsUnsupported; 
   			        details: 'GemStone currently does not support traits';
  			        object: self ; signal .
			      ^ nil.
    			  ].
		..
		
		but then 3 out of 4 issue247 tests in MCGsClassModificationTestCase fail (or error, from time to time)
		 "
		)