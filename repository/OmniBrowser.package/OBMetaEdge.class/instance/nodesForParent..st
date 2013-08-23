accessing
nodesForParent: aNode
	^ (aNode perform: selector) do: [:ea | ea metaNode: metaNode. ea navigation: self navigation]