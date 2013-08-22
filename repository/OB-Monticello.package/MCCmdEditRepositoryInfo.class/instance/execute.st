execution
execute
	| newRepo |
	newRepo := target repository obConfigure.
	newRepo ifNotNil: [ 
		newRepo class = self repository class
			ifTrue: [
				self repository copyFrom: newRepo.
				self doAutoCommit]
			ifFalse: [OBInformRequest message: 'Must not change repository type!']].
	self targetNodeChanged