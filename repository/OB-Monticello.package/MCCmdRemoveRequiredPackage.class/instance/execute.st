execution
execute
	self pickRequiredPackage ifNotNilDo:
		[:required |
		target workingCopy requiredPackages remove: required.
		self refresh]