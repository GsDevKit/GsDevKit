navigating
instanceVariables
	^ self theClass allInstVarNames asArray sort
		collect: [:ea | OBInstanceVariableNode on: ea inClass: self theClass]