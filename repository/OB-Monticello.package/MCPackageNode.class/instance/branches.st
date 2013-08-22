accessing
branches
	| branchDict selector n |
	selector _ name ifNil: [#packageBranch] ifNotNil: [#branch].
	branchDict _ Dictionary new.
	self versions do:
		[:ea |
		n _ ea perform: selector.
		(branchDict at: n ifAbsentPut:
			[(MCPackageNode name: n repository: repository) parent: self])
				addVersion: ea].
	^ branchDict values asSortedCollection: [:a :b | a name <= b name]