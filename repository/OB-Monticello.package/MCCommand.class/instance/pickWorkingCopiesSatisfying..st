actions
pickWorkingCopiesSatisfying: aBlock
	| copies item |
	copies := (MCWorkingCopy allManagers select: aBlock)
		asSortedCollection: [:a :b | a packageName <= b packageName].
	item := OBChoiceRequest 
		prompt: 'Package:' 
		labels: #('match ...'),(copies collect: [:ea | ea packageName])
		values: #(match), copies.
	item == #match ifTrue: [
		| pattern |
		pattern := OBTextRequest
			prompt: 'Packages matching:'
			template: '*'.
		^pattern isEmptyOrNil
			ifTrue: [#()]
			ifFalse: [
				(pattern includes: $*) ifFalse: [pattern _ '*', pattern, '*'].
				copies select: [:ea | pattern match: ea packageName]]
	].
	^ item == nil
		ifTrue: [#()]
		ifFalse: [{item}]