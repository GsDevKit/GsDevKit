public
doSilently: aBlock
	"Perform the block, and ensure that no system notification are broadcasted while doing so."

	self silenceLevel: (self silenceLevel + 1).
	aBlock ensure: [ self silenceLevel: (self silenceLevel - 1) ]