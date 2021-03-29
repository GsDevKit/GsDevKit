running
tearDown
  super tearDown.
  objectLogEnabled
    ifTrue: [ TranscriptProxy enableLoggingToObjectLogForSession ]
    ifFalse: [ TranscriptProxy disableLoggingToObjectLogForSession ].
  gemLogEnabled
    ifTrue: [ TranscriptProxy enableLoggingToGemLogFileForSession ]
    ifFalse: [ TranscriptProxy disableLoggingToGemLogFileForSession ]