private
doRemove
	| methods idxToBeRemoved method | 
	methods := OrderedCollection new.
	requestor previous 
	  ifNotNilDo: [:column | | selectedNode |		
		selectedNode := column selectedNode. 
		(selectedNode isClassNode or: [selectedNode isMethodCategoryNode]) ifTrue: [
			methods := selectedNode methods.
		     idxToBeRemoved := methods indexOf: target]]. 
	
	target theClass removeSelector: target selector.
	requestor announce: (OBNodeDeleted node: target).
	self doAutoCommit.

	methods size > 1 ifTrue: [
	    method := methods at: idxToBeRemoved + 1 
	                          ifAbsent: [methods at: idxToBeRemoved - 1]. 	
	    self select: method with: requestor announcer.	
	]