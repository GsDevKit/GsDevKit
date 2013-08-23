navigating
nodeHierarchyWithClass: aClass
	^ self surroundingHierarchy collect: [:ea | aClass on: ea]