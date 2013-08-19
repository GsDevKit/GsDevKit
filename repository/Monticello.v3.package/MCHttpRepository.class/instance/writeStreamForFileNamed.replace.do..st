required
writeStreamForFileNamed: aString replace: ignoreBoolean do: aBlock 
	| response  statusLine code |
	response := MCPlatformSupport
				writeStreamForFileNamed: (self urlForFileNamed: aString)
				user: user
				passwd: password
				replace: ignoreBoolean
				do: aBlock.
	"More robust handling of HTTP responses. Instead of enumerating
	all possible return codes and http versions, do a quick parse"
	(response beginsWith: 'HTTP/') ifTrue:[
		"Looks like an HTTP header, not some error message"
		statusLine := response copyUpTo: Character cr.
		code := [(statusLine findTokens: ' ') second asInteger] on: Error do:[].
	].
	(code isInteger and:[code between: 200 and: 299]) 
		ifFalse: [
			(self confirm: 'Error saving ', aString printString, ' to ', self description printString, '.', String cr, 'Try again with new userName and password? ')
			ifTrue: [ | newPass |
			newPass := self promptForUserNamePassword.
			newPass ~= password
				ifTrue: [
					self password: newPass.
					^self writeStreamForFileNamed: aString replace: ignoreBoolean do: aBlock ]]
			ifFalse: [self error: 'Error saving ', aString printString, ' to repository: ', String cr, response]]
