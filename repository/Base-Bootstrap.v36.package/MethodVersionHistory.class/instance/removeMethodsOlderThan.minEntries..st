private
removeMethodsOlderThan: aDateTime minEntries: aNumber
	"dont delete versions if number of old versions is lower than aNumber"
	self dictionary keysAndValuesDo: [:cl :methodDict | 
			methodDict values do: [:aColl | 
				aColl size > aNumber ifTrue: [aColl copy do: [:methodVersion | methodVersion asDateTime < aDateTime ifTrue: [aColl remove: methodVersion] ] ] ] ]