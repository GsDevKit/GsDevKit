access
query
	"return the query.  There is never a MuSwiki URL without a query; the query defaults to 'top' if none is explicitly specified"
	| q |
	q _ super query.
	q isNil ifTrue: [ q _ 'top' ].
	^q