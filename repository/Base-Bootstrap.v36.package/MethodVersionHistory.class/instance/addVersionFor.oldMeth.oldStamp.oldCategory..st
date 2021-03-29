actions
addVersionFor: aGsMethod oldMeth: oldMethod oldStamp: oldStamp oldCategory: oldCategory

	| cls selector source stamp mDict coll cat |
	cls := aGsMethod inClass.
	selector := aGsMethod selector.
	source :=  aGsMethod sourceString.
	stamp := cls stampForMethod: selector.
	cat := cls categoryOfSelector: selector.
	mDict := self dictionary at: cls ifAbsent: [ nil ].
	mDict == nil
		ifTrue: [
			mDict := Dictionary new.
			self dictionary at: cls put: mDict].
	coll := mDict at: selector ifAbsent: [ nil ].
	coll == nil 
		ifTrue: [
			coll := OrderedCollection new.
			oldMethod ~~ nil
				ifTrue: [
					coll addFirst: (MethodVersionRecord
							behavior: cls 
							selector: selector 
							category: oldCategory
							changeStamp: oldStamp 
							source: oldMethod sourceString)].
			mDict at: selector put: coll].
	coll addFirst: (MethodVersionRecord
				behavior: cls 
				selector: selector 
				category: cat
				changeStamp: stamp 
				source: source).