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
        ifFalse: [ 
          (gsMethod respondsTo: #'_stepPointForIp:level:isNative:')
            ifTrue: [ 
              gsMethod
                perform: #'_stepPointForIp:level:isNative:'
                withArguments:
                  {ip.
                  index.
                  (process _nativeStack)} ]
            ifFalse: [ 
              "v3.3"
              gsMethod
                perform: #'_stepPointForIp:level:useNext:'
                withArguments:
                  {ip.
                  index.
                  (process _nativeStack)} ] ] ]