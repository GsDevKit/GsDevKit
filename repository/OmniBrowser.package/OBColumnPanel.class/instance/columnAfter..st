accessing columns
columnAfter: aColumn 
	^ [self columns after: aColumn]
		on: Error
		do: [:err | nil]