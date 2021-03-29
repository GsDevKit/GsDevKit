*bootstrap-instance creation
defaultList: aCollection 
	"Answer an instance of me with initial elements from the argument, 
	aCollection."

| inst |
inst := super new.
inst classes: ClassSet new.
aCollection do: [:cl | inst classes add: cl ].
inst rebuildHierarchy.
^inst