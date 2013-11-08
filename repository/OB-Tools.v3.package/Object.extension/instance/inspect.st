*ob-tools-inspector
inspect

"If a client GUI not initialized, print to the  topaz -l output  , or if
 session not in a topaz -l process  print to gem log file."

(OBGemStonePlatform clientForwarderMap at:#browse otherwise: nil) ~~ nil ifTrue:[
  ^(OTInspector on: self) open
] ifFalse:[
  GsFile gciLogServer:  self asString .
  ^ self
]
