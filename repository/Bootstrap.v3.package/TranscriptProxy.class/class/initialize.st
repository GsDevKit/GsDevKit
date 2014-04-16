initialization
initialize
  "TranscriptProxy initialize"

  | userGlobals |
  userGlobals := GsSession currentSession symbolList objectNamed: #'UserGlobals'.
  userGlobals at: #'Transcript' put: self.
  self _resolveUndeclaredSymbolsFor: #'Transcript'