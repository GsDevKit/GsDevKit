session logging control
_enableLoggingForSession: attribute
  | skipLoggingObj symbolList ar skipLoggingDict |
  symbolList := GsSession currentSession symbolList.
  skipLoggingObj := (symbolList objectNamed: attribute) ifNil: [ ^ true ].
  ar := symbolList dictionaryAndSymbolOf: skipLoggingObj.
  skipLoggingDict := ar at: 1.
  symbolList remove: skipLoggingDict ifAbsent: [ ^ false ].
  ^ true