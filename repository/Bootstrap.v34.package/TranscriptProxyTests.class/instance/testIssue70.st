tests
testIssue70
  "https://github.com/GsDevKit/GsDevKit/issues/70"

  TranscriptProxy enableLoggingToObjectLogForSession.
  self assert: TranscriptProxy isLoggingToObjectLogForSessionEnabled.
  TranscriptProxy disableLoggingToObjectLogForSession.
  self deny: TranscriptProxy isLoggingToObjectLogForSessionEnabled.
  TranscriptProxy enableLoggingToGemLogFileForSession.
  self assert: TranscriptProxy isLoggingToGemLogFileForSessionEnabled.
  TranscriptProxy disableLoggingToGemLogFileForSession.
  self deny: TranscriptProxy isLoggingToGemLogFileForSessionEnabled