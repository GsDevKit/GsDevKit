*gemstone-indexing-extensions
_isValidRangeOperation: queryOp
	"Returns true if the given search operation is one of < <="

	^ queryOp == #'<' or: [ queryOp == #'<=']
