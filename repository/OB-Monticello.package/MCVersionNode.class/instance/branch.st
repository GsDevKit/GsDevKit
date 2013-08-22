accessing
branch
	| branch |
	branch _ self packageBranch copyAfter: $..
	branch isEmpty ifTrue: [^ ' --trunk--'].
	^ branch