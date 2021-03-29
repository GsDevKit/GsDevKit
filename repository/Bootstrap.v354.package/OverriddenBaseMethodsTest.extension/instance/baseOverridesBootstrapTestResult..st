*bootstrap
baseOverridesBootstrapTestResult: known

	self 
		addTo: known 
		forBehavior: TestResult 
		overrides: {
			#isError: -> 'isError: aTestCase

	^self errors includes: aTestCase
'.
			#failures -> 'failures
	failures == nil
		ifTrue: [failures := Set new].
	^failures
'.
			#passed -> 'passed

	passed == nil
		ifTrue: [passed := OrderedCollection new].

	^passed
'.
			#runCase: -> 'runCase: aTestCase

	[
		[
			aTestCase runCase.
			self passed add: aTestCase.
		] sunitOn: self class failure do: [:signal |
			self failures add: aTestCase.
			^self.
		].
	] sunitOn: self class error do: [:signal |
		self errors add: aTestCase.
		^self.
	].
'.
			#isPassed: -> 'isPassed: aTestCase

	^self passed includes: aTestCase
'.
			#isFailure: -> 'isFailure: aTestCase
	^self failures includes: aTestCase
'.
			#initialize -> 'initialize
'.
			#printOn: -> 'printOn: aStream

	aStream
		nextPutAll: self runCount printString;
		nextPutAll: '' run, '';
		nextPutAll: self correctCount printString;
		nextPutAll: '' passed, '';
		nextPutAll: self failureCount printString;
		nextPutAll: '' failed, '';
		nextPutAll: self errorCount printString;
		nextPutAll: '' error''.

	self errorCount ~~ 1
		ifTrue: [aStream nextPut: $s]
'.
			#errors -> 'errors

	errors == nil
		ifTrue: [errors := OrderedCollection new].
	^errors
'.
	}