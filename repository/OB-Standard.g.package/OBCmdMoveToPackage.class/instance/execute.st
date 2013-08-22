execution
execute
	| tmpPackages packageNames packages lines typeLabel answer |
	typeLabel := 'type...'.	
	packages := tmpPackages := PackageOrganizer default packages 
					asSortedCollection: [:a :b | a packageName <= b packageName].
	packageNames := (packages := tmpPackages reject: [:pkg | pkg = target theNonMetaClass package]) collect: [:ea | ea packageName].
	lines := #().
	
	target isClassNode ifTrue: [
	    lines := #(1).	
	    packages := OrderedCollection new.	
	    packageNames := OrderedCollection new.
	    packages add: typeLabel. packageNames add: typeLabel.
	    tmpPackages do: [:pkg | packages add: pkg. packageNames add: pkg packageName.
		                           pkg systemCategories ifNotEmptyDo: [:cats |
		                               cats allButFirst do: [:cat | packages add: cat. packageNames add: '   ', cat]]].	
	].
	
	
	answer := OBChoiceRequest prompt: nil labels: packageNames values: packages lines: lines.
	answer ifNotNil: [
		target isClassNode 
		    ifTrue: [
			   answer = typeLabel ifTrue: [
				   answer := self requestPackageToUser.
				   (packageNames anySatisfy: [:pkgName | pkgName withoutLeadingBlanks = answer]) 
			            ifFalse: [self inform: 'This package or class category does not exist!'. ^self]].
			   target theNonMetaClass category: (answer isString ifFalse: [answer packageName] ifTrue: [answer])] 
		    ifFalse: [answer addMethod: target reference].
		].
	self doAutoCommit