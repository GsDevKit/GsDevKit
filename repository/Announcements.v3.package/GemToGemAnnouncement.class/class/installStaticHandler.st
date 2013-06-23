announcing
installStaticHandler
  | handler |
  self registeredSessions add: GsCurrentSession currentSession serialNumber.
  handler := InterSessionSignal addDefaultHandler: [:ex |
     GemToGemAnnouncement signal: ex sentInt 
               message: ex sentMessage 
	      sessionSerialNum: ex sendingSession .
     System enableSignaledGemStoneSessionError .
     ex resume
  ].
  SessionTemps current at: #GemToGemStaticException put: handler.
  System enableSignaledGemStoneSessionError.
