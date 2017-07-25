special cases
nonImplementedSelectorsBaseBugs

	^#(compareStartingAt:caseSensitiveEquals: _idxForCompareLessThanOrEqual: 
		_idxForCompareGreaterThanOrEqual: indicatesNsc transformIntoDateTime: iftrue: #'!='
		signalEarlyReturn 
		_dayOfYear				"Bug41972"
		convertTo5				"Bug41973"
		transformIntoSymbol	"Bug41974"
	)