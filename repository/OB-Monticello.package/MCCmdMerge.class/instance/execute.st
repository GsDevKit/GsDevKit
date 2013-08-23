execution
execute
	| versionMerger |
	versionMerger _ MCVersionMerger new addVersion: target version.
	versionMerger applyRecords.
	versionMerger isAncestorMerge ifTrue: [^ OBInformRequest message: 'No changes'].
	MCMergeTool openRoot: (MCVersionMergerNode new versionMerger: versionMerger).