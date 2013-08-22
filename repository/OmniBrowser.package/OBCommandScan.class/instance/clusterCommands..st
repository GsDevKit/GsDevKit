user interface
clusterCommands: aCollection
	| groups root name cluster |
	groups _ IdentityDictionary new.
	root _ OBCommandCluster new.
	aCollection do:
		[:command |
		name _ command cluster.
		cluster _ name 
					ifNil: [root]
					ifNotNil: 
						[groups 
							at: name
							ifAbsentPut: [command createCluster]].
		cluster addCommand: command].
	groups values do: [:ea | root addCommand: ea].
	^ root