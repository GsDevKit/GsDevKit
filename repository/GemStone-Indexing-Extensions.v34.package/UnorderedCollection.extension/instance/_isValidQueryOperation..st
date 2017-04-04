*gemstone-indexing-extensions
_isValidQueryOperation: queryOp
	"Returns true if the given search operation is one of == ~~ < > = <= >= or ~=."

	^ #( #'==' #'~~' #'=' #'~=' #< #'<=' #> #'>=' ) includes: queryOp
