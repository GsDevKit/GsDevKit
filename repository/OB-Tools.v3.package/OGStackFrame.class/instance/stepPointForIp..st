private
stepPointForIp: ip
  ^ breakpointOnStack
    ifTrue: [ gsMethod _nextStepPointForIp: ip ]
    ifFalse: [ 
      (gsMethod respondsTo: #'_stepPointForIp:level:isNative:')
        ifTrue: [ gsMethod _stepPointForIp: ip level: index isNative: process _nativeStack ]
        ifFalse: [ gsMethod _stepPointForIp: ip level: index useNext: process _nativeStack ] ]