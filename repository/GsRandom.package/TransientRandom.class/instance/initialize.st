Private
initialize
	"adjust the default seed by session serial number so that vms started at same time won't get same seed"
	"*** obi *** avoid serial numbers > 32768 > ((GsSession serialOfSession: System session) bitShift: 16))"
	super initialize.
	self seed: (seed bitXor: (System session bitShift: 16)).
