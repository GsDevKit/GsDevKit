initialization
initialize
	"self initialize"
	
	RegisteredSessions := RcIdentityBag new: 100.
	Registry := Dictionary new.
	self allSubclasses do: [:cl | cl initialize].