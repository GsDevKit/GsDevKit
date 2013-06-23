instance creation
sendFrom: aMailAddressFrom to: aMailAddress subject: aSubjectString text: aText 
	^self sendFrom: aMailAddressFrom to: aMailAddress cc: '' subject: aSubjectString text: aText