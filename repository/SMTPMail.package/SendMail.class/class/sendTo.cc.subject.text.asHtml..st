instance creation
sendTo: aMailAddress cc: otherMailAddresses subject: aSubjectString text: aText asHtml: aBoolean
	| newMail |
	newMail := self new.
	aBoolean ifTrue: [ newMail setContentTypeHtml ].
	newMail
		to: aMailAddress;
		cc: otherMailAddresses;
		subject: aSubjectString;
		text: aText.
	^newMail send
 

