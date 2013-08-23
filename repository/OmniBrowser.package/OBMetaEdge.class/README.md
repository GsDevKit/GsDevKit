An OBMetaEdge is an edge in the browser's metagraph. It represents a message sent to a node to obtain further nodes. It is refered to by the "parent" metanode, and refers to the "child" metanode.

iVars:

label 		-  a string describing the metaNode, for filters which allow the user 
			   to choose which edges to follow
selector	- when a node is selected by the user, this message will be 
			  sent to it to obtain its children
metaNode 	- a MetaNode corresponding to the nodes answered by the above message
		