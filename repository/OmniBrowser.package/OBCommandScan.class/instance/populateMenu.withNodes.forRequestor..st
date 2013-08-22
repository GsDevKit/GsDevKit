user interface
populateMenu: aMenu withNodes: aCollection forRequestor: aRequestor
	| commands cluster |
	commands _ IdentitySet new.
	aCollection do: [:node | self addCommandsOn: node for: aRequestor to: commands].
	cluster _ self clusterCommands: commands.
	cluster populateMenu: aMenu