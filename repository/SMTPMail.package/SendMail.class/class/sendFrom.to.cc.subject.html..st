instance creation
sendFrom: aMailAddressFrom to: aMailAddress cc: otherMailAddresses subject: aSubjectString html: aText 
	^self sendFrom: aMailAddressFrom to: aMailAddress cc: otherMailAddresses subject: aSubjectString text: aText asHtml: true
