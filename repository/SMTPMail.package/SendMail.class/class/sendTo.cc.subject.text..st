instance creation
sendTo: aMailAddress cc: otherMailAddresses subject: aSubjectString text: aText
	^self sendTo: aMailAddress cc: otherMailAddresses subject: aSubjectString text: aText asHtml: false
 

