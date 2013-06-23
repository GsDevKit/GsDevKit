accessing
truncate: pos
	"Truncate to this position"

	self position: pos.
	self gsfile flush.
	self close.
	self gsfile open.
	self position: pos
