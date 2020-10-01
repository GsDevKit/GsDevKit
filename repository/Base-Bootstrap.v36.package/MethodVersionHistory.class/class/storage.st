private
storage

	^self shouldPersistHistory
		ifTrue: [ GsSession currentSession objectNamed: #UserGlobals ]
		ifFalse: [ SessionTemps current ]