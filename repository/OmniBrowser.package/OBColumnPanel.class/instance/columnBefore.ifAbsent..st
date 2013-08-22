accessing columns
columnBefore: aColumn ifAbsent: aBlock
	^ [self columns before: aColumn]
		on: Error 
		do: [:err | aBlock value]