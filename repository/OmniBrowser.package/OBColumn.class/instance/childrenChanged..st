updating
childrenChanged: announcement
	(self parent = announcement node) ifTrue:
		[self refresh]