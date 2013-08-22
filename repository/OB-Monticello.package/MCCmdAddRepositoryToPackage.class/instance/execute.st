execution
execute

		(self pickWorkingCopySatisfying: [ :p | (p repositoryGroup includes: self repository) not ]) ifNotNilDo:
			[:wc |
			wc repositoryGroup addRepository: self repository.
			self doAutoCommit]