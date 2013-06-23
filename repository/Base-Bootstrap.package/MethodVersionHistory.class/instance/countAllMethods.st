private
countAllMethods
	| sum | 
	sum := 0.
	self dictionary keysAndValuesDo: [:cl :methodDict | methodDict values do: [:aColl | sum := sum + aColl size]].
	^sum