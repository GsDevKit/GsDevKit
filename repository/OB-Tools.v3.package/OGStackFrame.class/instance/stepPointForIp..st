private
stepPointForIp: ip

	^breakpointOnStack
		ifTrue: [ gsMethod _nextStepPointForIp: ip ]
		ifFalse: [ gsMethod _stepPointForIp: ip level: index isNative: process _nativeStack ]