tests
testAddRemoveMethod

	| history def class versions |
	history := MethodVersionHistory uniqueInstance.
	class := self mockClass.
	versions := history versionsOfMethod: self methodSelector in: class.
	self assert: versions isEmpty.

	"class"
	(def := self methodDefinition: false) load.
	versions := history versionsOfMethod: self methodSelector in: class.
	self assert: versions size == 1.
	
	def unload.
	versions := history versionsOfMethod: self methodSelector in: class.
	self assert: versions isEmpty.

	"metaClass"
	versions := history versionsOfMethod: self methodSelector in: class class.
	self assert: versions isEmpty.

	(def := self methodDefinition: true) load.
	versions := history versionsOfMethod: self methodSelector in: class class.
	self assert: versions size == 1.

	def unload.
	versions := history versionsOfMethod: self methodSelector in: class class.
	self assert: versions isEmpty.
