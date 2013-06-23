error handling
_errorSymbolToNumber: errorSymbol args: argList

"Translates the errorSymbol to an error number.
 returns the error number (a SmallInteger, and may modify argList."

 ^ ErrorSymbols at: errorSymbol
          ifAbsent: [ argList insertObject: errorSymbol at: 1 .
                        "hard code error number to ensure we can survive a
			 with damaged error dictionary"
                      2063 "ErrorSymbols at: #rtErrNoMessage " .
                    ].
