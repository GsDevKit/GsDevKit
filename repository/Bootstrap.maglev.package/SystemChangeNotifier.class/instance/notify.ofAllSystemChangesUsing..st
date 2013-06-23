public
notify: anObject ofAllSystemChangesUsing: oneArgumentSelector 
	"Notifies an object of any system changes."

	self
		observe: SystemChangeAnnouncement
		send: oneArgumentSelector
		to: anObject