accessing columns
columnBefore: aColumn 
	^ self columnBefore: aColumn ifAbsent: [nil]