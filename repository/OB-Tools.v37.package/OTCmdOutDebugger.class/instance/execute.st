execution
execute
	| current startContext |
	startContext := current := self context.
	[ current = startContext ]
		whileTrue: [ 
			self process completeStep: current.
			current := self process suspendedContext ].
	self update