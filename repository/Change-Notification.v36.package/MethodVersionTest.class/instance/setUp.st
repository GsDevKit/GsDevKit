running
setUp

	super setUp.
	classDefinition := self mockClass: 'MethodVersionMockClass' super: 'Object'.
	classDefinition load.
