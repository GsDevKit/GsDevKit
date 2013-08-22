accessing
selectionPath
	^ ((self columns collect: [ :e | e parent ]) select: [ :e | e notNil ]) allButFirst