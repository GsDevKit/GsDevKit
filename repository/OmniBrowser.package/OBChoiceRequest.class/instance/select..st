signaling
select: aString
	| index |
	index _ labels indexOf: aString.
	self resume: (values at: index ifAbsent: [nil])