private
statusColor
	result == nil ifTrue: [ ^Color white ].
	result hasPassed 
		ifTrue:[ ^ Color green ].
	result hasErrors 
		ifTrue: [ ^ Color red ].
	^ Color yellow.