execution
execute
	| current stepPoint cm startLevel startContext |
	cm := self getCompiledMethod.
	cm == nil ifTrue: [ ^self ].
	stepPoint := self findStepPointIn: cm.
	startContext := current := self context.
	[ | home |
	  home := current homeContext.
	  home = startContext
		ifTrue: [ stepPoint ~~ (self process _localStepPointAt: current level) ]
		ifFalse: [ current inSenderChain: startContext at: stepPoint]]
		whileTrue: [ 
			self process completeStep: current.
			current := self process suspendedContext ].
	self update