private
removeNotImplementedMethods
	"remove methods which are not part of my method dicts"
	self dictionary keysAndValuesDo: [:cl :methodDict | 
			methodDict keys do: [:selector | 
				 (cl compiledMethodAt: selector ifAbsent: []) == nil ifTrue: [methodDict removeKey: selector]] ]