callbacks
drop: passenger on: target
	^ [target acceptDroppedNode: passenger] 
		on: OBAnnouncerRequest
		do: [:notification | notification resume: self announcer]