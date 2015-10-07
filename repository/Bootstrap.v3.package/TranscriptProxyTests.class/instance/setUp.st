running
setUp
  super setUp.
  gemLogEnabled := TranscriptProxy isLoggingToGemLogFileForSessionEnabled.
  objectLogEnabled := TranscriptProxy isLoggingToObjectLogForSessionEnabled