evaluating
valueWithArguments: anArray 
	| answer |
	self
		do: [:each | answer := each valueWithArguments: anArray].
	^ answer