configuration
defaultMetaNode
	| allPackages packages repositories |
	allPackages := OBMetaNode named: 'All packages'.
	packages := OBMetaNode named: 'Packages'.
	allPackages childAt: #packages put: packages.
	packages addFilter: MCModifiedFilter new.
	repositories := OBMetaNode named: 'Repositories'.
	packages childAt: #repositories put: repositories.
	^ allPackages