tests
testRemoveClass

	| history class versions |
	history := MethodVersionHistory uniqueInstance.
	class := self mockClass.
	versions := history versionsOfMethod: self methodSelector in: class.
	self assert: versions isEmpty.
	(self methodDefinition: false) load.
	versions := history versionsOfMethod: self methodSelector in: class.
	self assert: versions size == 1.

	[class removeFromSystem.
	versions := history versionsOfMethod: self methodSelector in: class.
	self assert: versions isEmpty] 
		ensure: [ self classDefinition load ]