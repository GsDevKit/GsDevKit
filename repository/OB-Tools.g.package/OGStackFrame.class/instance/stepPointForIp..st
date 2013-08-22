private
stepPointForIp: ip

	^breakpointOnStack
		ifTrue: [ gsMethod _nextStepPointForIp: ip quick: false ]
		ifFalse: [ gsMethod _stepPointForIp: ip level: index quick: false ]