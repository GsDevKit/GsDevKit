*gemstone-indexing-extensions
_isRangeEqualityOperation: queryOp
	"Returns true if the given search operation is one of < > = <= >= or ~=."

	^ queryOp ~~#'==' and: [ queryOp ~~ #'~~']
