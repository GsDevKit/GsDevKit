tests
testCreationMethods
	"Tests creation via MCRepository>>location:"

	| repo |

	repo := MCRepository location: FileDirectory default pathName.
	self assert: (repo isKindOf: MCDirectoryRepository).
	self assert: (repo description = FileDirectory default pathName).

	repo := MCRepository location: (FileDirectory default pathName, FileDirectory slash, '*').
	self assert: (repo isKindOf: MCSubDirectoryRepository).
	"XXXX: MCSubDirectoryRepo prints wrongly on Windows"
	self assert: (repo description = (FileDirectory default pathName, FileDirectory slash, '*')).

	repo := MCRepository location: 'http://source.squeak.org/trunk'.
	self assert: (repo isKindOf: MCHttpRepository).
	self assert: (repo description = 'http://source.squeak.org/trunk').

"	repo := MCRepository 
		location: 'ftp://source.squeak.org/trunk' 
		username: 'foo' 
		password: 'bar'.
	self assert: (repo isKindOf: MCFtpRepository).
	self assert: (repo description = 'ftp://foo@source.squeak.org/trunk').
"