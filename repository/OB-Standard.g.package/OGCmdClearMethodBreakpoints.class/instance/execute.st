action
execute

	| cm cls sel |
	cm := self getCompiledMethod.
	cm == nil ifTrue: [ ^self ].
	cls := cm inClass.
	sel := cm selector.
	OTRemoteDebugger sendSetOrClearBreakPointSignal: 
		'(', cls name, ' compiledMethodAt: ', sel printString,') clearAllBreaks'.
	cm clearAllBreaks.
	self update.