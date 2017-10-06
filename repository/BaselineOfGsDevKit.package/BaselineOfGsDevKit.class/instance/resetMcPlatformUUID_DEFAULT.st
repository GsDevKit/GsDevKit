doits
resetMcPlatformUUID_DEFAULT
  Smalltalk
    at: #'MCPlatformSupport'
    ifPresent: [ :cl | cl transientGlobalForKey: #'UUID_DEFUALT' value: nil ]