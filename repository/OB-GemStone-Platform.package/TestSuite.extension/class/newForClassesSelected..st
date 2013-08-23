*ob-gemstone-platform-GSExtensions
newForClassesSelected: classesSelected
	| instance |
	instance := self new.
	classesSelected do: [ :aTestClass |
		aTestClass isAbstract
			ifFalse: [ aTestClass addToSuiteFromSelectors: instance ]].
	^instance