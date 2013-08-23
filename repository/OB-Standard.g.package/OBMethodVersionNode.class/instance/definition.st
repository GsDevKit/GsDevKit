compiling
definition
	^ (OBMethodDefinition source: version source inClass: self theClass)
		callback: [:sel | 
					version selector = sel
						ifTrue: [self class on: version latest]
						ifFalse: [OBMethodNode on: sel inClass: self theClass]]