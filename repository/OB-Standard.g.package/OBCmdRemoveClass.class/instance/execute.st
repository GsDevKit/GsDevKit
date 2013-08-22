execution
execute
	(OBConfirmationRequest 
		prompt: 'Are you certain that you 
want to REMOVE the class ' 
				, target theNonMetaClassName , ' from the system?'
		confirm: 'Remove') ifFalse: [^ self].
	target theNonMetaClass removeFromSystem.
	target signalDeletion.
	self doAutoCommit