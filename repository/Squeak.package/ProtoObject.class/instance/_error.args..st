error handling
_error: errorSymbol args: argList

"Private Error Messages."

| newArgList errorNumber|

newArgList := #[ self ] addAll: argList ; yourself .
errorNumber := self _errorSymbolToNumber: errorSymbol args: newArgList .

^ System signal: errorNumber args: newArgList signalDictionary: GemStoneError
