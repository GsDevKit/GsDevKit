as yet unclassified
defaultMetaNode
	| repos versions root branches packages |
	root _ OBMetaNode named: 'root'.
	repos _ OBMetaNode named: 'repositories'.
	packages _ OBMetaNode named: 'packages'.
	branches _ OBMetaNode named: 'branches'.
	versions _ OBMetaNode named: 'bersions'.
		
	root - #fileBasedRepositories -> repos.
	repos - #packagesByName -> packages.
	packages - #branches -> branches.
	branches - #sortedVersions -> versions.
	
	packages autoSelect: branches.
	packages addFilter: MCModifiedFilter new.
	^ root