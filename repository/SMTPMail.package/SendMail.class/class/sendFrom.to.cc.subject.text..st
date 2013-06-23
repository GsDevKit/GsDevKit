instance creation
sendFrom: aMailAddressFrom to: aMailAddress cc: otherMailAddresses subject: aSubjectString text: aText 
	^self sendFrom: aMailAddressFrom to: aMailAddress cc: otherMailAddresses subject: aSubjectString text: aText asHtml: false
