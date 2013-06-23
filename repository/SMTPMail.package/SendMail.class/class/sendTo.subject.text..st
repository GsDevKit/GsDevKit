instance creation
sendTo: aMailAddress subject: aSubjectString text: aText
	^self sendTo: aMailAddress cc: '' subject: aSubjectString text: aText

