session logging control
_isLoggingForSessionEnabled: attribute
  ^ (GsSession currentSession symbolList objectNamed: attribute) isNil