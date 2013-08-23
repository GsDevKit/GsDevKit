updating
columnSelected: announcement
	"This method is sent by the browser when a node is selected. The annoucement is an OBSelectionChanged."
	(announcement node isKindOf: OBClassNode)
		ifTrue: [self userHasSelectedClass: announcement node theNonMetaClassName]