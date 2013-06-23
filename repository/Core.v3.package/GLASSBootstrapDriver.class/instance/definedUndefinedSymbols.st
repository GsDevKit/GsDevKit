monticello installing
definedUndefinedSymbols

	| undef |
	undef := SymbolDictionary new.
	undef name: #UndefinedSymbols.
	UserGlobals at: #UndefinedSymbols put: undef.
	UserGlobals at: #UndefinedSymbolList put: (SymbolList with: undef).
