private
saveToFile
	| file | file := GsFile openWriteOnServer: 'mail_', (DateTime now asString copyReplacing: $  with: $_), '.txt'.
	file nextPutAll: 'To: ', to.
	file cr.
	file nextPutAll: 'Subject: ', subject.
	file cr; cr.
	file nextPutAll: text.
	file close.
	^true