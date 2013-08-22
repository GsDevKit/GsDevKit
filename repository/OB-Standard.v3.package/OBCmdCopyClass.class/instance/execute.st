execution
execute

	| newName newClass |
	newName := OBTextRequest
			prompt: 'Please type new class name'
			template: target theNonMetaClassName asString.
	newName ifNotNil:	
		[OBPlatform current doAutoAbort.
		newClass := self
						copyClass: target theNonMetaClass 
						as: newName asSymbol.
		self select: newClass asNode with: requestor announcer].
	self doAutoCommit