actions
refresh
	| packageNames name latest av |
	packageNames := Set new.
	versions := self repository versionNodes collect: [ :node | | ar |
		ar := node nameComponents.
		(ar at: 2) isEmpty ifFalse: [ packageNames add: (ar at: 1) ].
		ar].
	packageDictionary ~~ nil
		ifTrue: [
			versions do: [:ar | | packageNode |
				packageNode := packageDictionary at: ar first ifAbsentPut:
									[(MCPackageNode name: ar first repository: repository)
										repositoryNode: self;
										packageName: ar last packageName;
										yourself].
				(packageNode versions includes: ar last) ifFalse: [ packageNode addVersion: ar last ]]].
	newer := Set new.
	inherited := Set new.
	loaded := Set new.
	(MCWorkingCopy allManagers 
"		select: [ :each | packageNames includes: each packageName]")
		do: [:each |
			each ancestors do: [ :ancestor |
				loaded add: ancestor name.
				ancestor ancestorsDoWhileTrue: [:heir |
					(inherited includes: heir name)
						ifTrue: [false]
						ifFalse: [inherited add: heir name. true]]].
			latest := (versions select: [:v | (v first copyUpTo: $.) = each package name])	
				detectMax: [:v | v third].
			(latest notNil and: [
				each ancestors allSatisfy: [:ancestor |
					av := [((ancestor name copyAfterLast: $-) copyAfterLast: $.) asInteger] on: Error do: [:ex | ex return: 0].
					av < latest third or: [
						av = latest third and: [((ancestor name copyAfterLast: $-) copyUpTo: $.) ~= latest second]]]])
				ifTrue: [newer add: each package name ]].