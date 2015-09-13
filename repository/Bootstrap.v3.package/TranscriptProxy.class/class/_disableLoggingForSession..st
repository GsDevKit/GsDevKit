session logging control
_disableLoggingForSession: attribute
  | skipLoggingDict symbolList |
  skipLoggingDict := SymbolDictionary new.
  skipLoggingDict at: attribute put: Object new.
  symbolList := GsSession currentSession symbolList.
  symbolList add: skipLoggingDict before: UserGlobals