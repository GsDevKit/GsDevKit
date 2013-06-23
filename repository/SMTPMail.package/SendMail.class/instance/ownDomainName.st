Accessing
ownDomainName
	^ownDomainName ~~ nil ifTrue: [ ownDomainName ] ifFalse: [ self class getLocalHostName ]