tokenizing
nextCDataContent
	| cdata |
	"Skip $[ "
	self next.
	cdata _ self nextUpToAll: ']]>'.
	self handleCData: cdata