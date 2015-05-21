private
stepPointForIp: ip
  ^ breakpointOnStack
    ifTrue: [ gsMethod _nextStepPointForIp: ip ]
    ifFalse: [ 
      (gsMethod respondsTo: #'_stepPointForIp:level:isNative:')
        ifTrue: [ 
          gsMethod
            perform: #'_stepPointForIp:level:isNative:'
            withArguments:
              {ip.
              index.
              (process _nativeStack)} ]
        ifFalse: [ gsMethod _stepPointForIp: ip level: index useNext: process _nativeStack ] ]