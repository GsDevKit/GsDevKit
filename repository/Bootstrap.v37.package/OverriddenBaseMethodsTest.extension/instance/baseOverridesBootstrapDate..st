*bootstrap
baseOverridesBootstrapDate: known
	self
		addTo: known
		forBehavior: Date
		overrides: {
			#'USDateFormat' -> 'USDateFormat

"Returns a String that expresses the date of the receiver. The date is in 
 United States format, month first (MM/DD/YY)."

^self asStringUsingFormat: #(2 1 3 $/ 1 2 $: false )
'.
			#'asString' -> 'asString

"Returns a String that expresses the receiver in the default format
 (DD/MM/YYYY)."

| t result |

t := self _yearMonthDay.
result := (t at: 3) _digitsAsString .
result addAll: ''/'';
  addAll: (t at: 2) _digitsAsString;
  addAll: ''/'';
  addAll: (t at: 1) _digitsAsString.
^ result
'.
	}