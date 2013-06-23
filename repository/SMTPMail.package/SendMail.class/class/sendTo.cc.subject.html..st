instance creation
sendTo: aMailAddress cc: otherMailAddresses subject: aSubjectString html: aText
	^self sendTo: aMailAddress cc: otherMailAddresses subject: aSubjectString text: aText asHtml: true
 

