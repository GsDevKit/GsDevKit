configuration
defaultMetaNode
	| packages repository versions |
	repository := OBMetaNode named: 'repository'.
	packages := OBMetaNode named: 'packages'.
	versions := OBMetaNode named: 'version'.
	repository childAt: #packagesHighlighted put: packages.
	packages childAt: #sortedVersions put: versions.
	^ repository