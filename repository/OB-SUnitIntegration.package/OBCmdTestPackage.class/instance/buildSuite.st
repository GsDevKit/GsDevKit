private
buildSuite
	^ target classes
		inject: TestSuite new
		into: [:suite :classNode | 
				(classNode theClass inheritsFrom: TestCase)
						ifTrue: [suite addTests: classNode theClass suite tests].
				suite]