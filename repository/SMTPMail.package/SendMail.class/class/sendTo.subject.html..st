instance creation
sendTo: aMailAddress subject: aSubjectString html: aText
	^self sendTo: aMailAddress cc: '' subject: aSubjectString html: aText

