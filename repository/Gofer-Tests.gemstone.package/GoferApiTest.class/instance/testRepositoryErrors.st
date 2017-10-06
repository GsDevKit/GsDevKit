testing-repositories-options
testRepositoryErrors
	gofer 
		url: 'http://xyzzyfoobarblam.com';
		repository: self monticelloRepository.
	gofer package: 'GoferFoo'.
	
	gofer enableRepositoryErrors.
	self should: [ gofer resolved ] raise: GoferRepositoryError.
	
	gofer disableRepositoryErrors.
	self shouldnt: [ gofer resolved ] raise: GoferRepositoryError