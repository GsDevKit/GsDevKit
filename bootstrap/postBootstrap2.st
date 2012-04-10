run
| undef |
undef := SymbolDictionary new.
undef name: #UndefinedSymbols.
UserGlobals at: #UndefinedSymbols put: undef.
UserGlobals at: #UndefinedSymbolList put: (SymbolList with: undef).
true
%
! shouldn't have to do this. It will do for now, though.
run
GsPackagePolicy current refreshSessionMethodDictionary.
true
%
