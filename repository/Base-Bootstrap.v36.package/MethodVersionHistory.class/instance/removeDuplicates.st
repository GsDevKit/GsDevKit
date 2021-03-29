actions
removeDuplicates
	"remove methods with same source"
	self dictionary keysAndValuesDo: [:cl :methodDict | 
			methodDict values do: [:aColl | | sourceDict |
				sourceDict := StringKeyValueDictionary new.
				aColl copy do: [:methodVersion | (sourceDict includesKey: methodVersion source)
													ifTrue: [ aColl remove: methodVersion]
													ifFalse: [sourceDict at: methodVersion source put: true] ] ] ]