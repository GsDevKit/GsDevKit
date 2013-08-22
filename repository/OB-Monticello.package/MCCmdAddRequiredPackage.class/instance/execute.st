execution
execute
	(self pickWorkingCopySatisfying: [:aWorkingCopy | self selectWorkingCopyAsRequiredCandidate: aWorkingCopy]) ifNotNilDo:
		[:wc |
		target workingCopy requirePackage: wc package.
		self refresh]