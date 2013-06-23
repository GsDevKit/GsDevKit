instance creation
sendFrom: aMailAddressFrom to: aMailAddress cc: otherMailAddresses subject: aSubjectString text: aText asHtml: aBoolean
	| newMail |
	newMail := self new.
	aBoolean ifTrue: [ newMail setContentTypeHtml ].
	newMail
		from: aMailAddressFrom; 
		to: aMailAddress;
		cc: otherMailAddresses;
		subject: aSubjectString;
		text: aText.
	^newMail send
