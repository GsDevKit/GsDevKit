tests
testMissingClass

	| history def class versions |
	history := MethodVersionHistory uniqueInstance.
	versions := history versionsOfMethod: #noMethod in: Object new.
	self assert: versions isEmpty.
