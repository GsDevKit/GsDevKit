private
sendHelo

	| helo rc |
	helo := self heloString.
	
	rc := self sendCrlfString: helo, self ownDomainName.
	(rc and: [self user ~~ nil]) ifTrue: [
		rc ifTrue: [rc := self readSmtpResult].
		rc ifTrue: [rc := self sendCrlfString: 'AUTH LOGIN ', (self encodeString: self user)].
		rc ifTrue: [rc := self readSmtpResult].
		rc ifTrue: [rc := self sendCrlfString: (self encodeString: self password)]].
	^ rc	
