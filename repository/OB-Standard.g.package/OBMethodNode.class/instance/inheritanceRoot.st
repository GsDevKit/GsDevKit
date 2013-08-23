private
inheritanceRoot
	| rootClass |
	rootClass := (self theClass withAllSuperclasses asArray
					select: [:ea | ea includesSelector: self selector]) last.
	^ OBCollectionNode on: {OBMethodNode 
								on: self selector 
								inClass: rootClass}