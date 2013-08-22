execution
execute
	| category |
	category := OBTextRequest
				prompt: 'Please type new category name' 
				template: target name.
	category ifNotNil:	
		[target container organization renameCategory: target name toBe: category.
		target signalChanged.
		self doAutoCommit]