private
countMethodsOlderThan: aDateTime
	| sum | 
	sum := 0.
	self dictionary keysAndValuesDo: [:cl :methodDict | methodDict values do: [:aColl | sum := sum + (aColl select: [:methodVersion | methodVersion asDateTime < aDateTime]) size]].
	^sum