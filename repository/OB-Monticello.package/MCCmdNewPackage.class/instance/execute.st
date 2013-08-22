execution
execute
	| name workingCopy |
	name := OBTextRequest
			prompt: 'Name of package:'
			template: ''.
	name isEmptyOrNil ifFalse:
		[PackageInfo registerPackageName: name.
		workingCopy := MCWorkingCopy forPackage: (MCPackage new name: name).
		self refresh].