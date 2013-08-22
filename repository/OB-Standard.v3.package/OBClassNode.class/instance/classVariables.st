navigating
classVariables
	^ self theClass allClassVarNames asArray sort
		collect: [:ea | OBClassVariableNode on: ea inClass: self theClass]