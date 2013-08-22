as yet unclassified
execute

	| sel cls cm stepPoint |
	cm := self getCompiledMethod.
	cm == nil ifTrue: [ ^self ].
	stepPoint := self findStepPointIn: cm.
	cls := cm inClass.
	sel := cm selector.
	OTRemoteDebugger sendSetOrClearBreakPointSignal: 
		cls name, ' _setMethodBreak: ',
		sel printString, ' stepPoint: ',
		stepPoint asString.
	(cls _setMethodBreak: sel stepPoint: stepPoint) == true ifFalse: [ self error: 'Breakpoint not set' ].
	self update.