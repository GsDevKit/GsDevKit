running
setUp
	| repo |
	super setUp.
	gofer := (Smalltalk at: #Gofer) new.
	gofer disablePackageCache.
	repo := self monticelloRepository.
	gofer repository: repo.
	Smalltalk at: #'GsDeployer_Test_Repository' put: repo.
