transcript
show: anObject
  | proxy |
  self isLoggingToObjectLogForSessionEnabled
    ifTrue: [ (ObjectLogEntry transcript: anObject printString object: anObject) addToLog ].
  (proxy := self proxyTranscript) ~~ nil
    ifTrue: [ proxy show: anObject ]
    ifFalse: [ 
      self isLoggingToGemLogFileForSessionEnabled
        ifTrue: [ GsFile gciLogServer: '--transcript--' , anObject printString ] ]