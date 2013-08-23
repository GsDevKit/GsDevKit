ancestry
isAncestorOf: aNode 
    ^ collection anySatisfy: [:ea | ea = aNode or: [ea isAncestorOf: aNode]]