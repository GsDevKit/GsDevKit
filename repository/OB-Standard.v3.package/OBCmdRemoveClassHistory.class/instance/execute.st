execution
execute
	(OBConfirmationRequest 
		prompt: 'Really remove class history from "' 
				, target name , '"?'
		confirm: 'Remove') ifFalse: [^ self].
	target removeClassHistory