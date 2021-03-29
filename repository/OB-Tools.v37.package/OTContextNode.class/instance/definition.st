accessing-dynamic
definition
	^ (OBMethodDefinition selection: self selection source: self source inClass: self theClass)
		callback: [ :selector | self compiled: selector ]