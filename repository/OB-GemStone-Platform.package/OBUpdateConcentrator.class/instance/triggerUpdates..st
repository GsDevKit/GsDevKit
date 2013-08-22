private
triggerUpdates: updateArray

	clientUpdateConcentrator ~~ nil
		ifTrue: [ 
			| ar coll stream |
			coll := Array new.
			ar := self concentrateUpdates: updateArray.
			ar isEmpty ifTrue: [ ^self ].
			ar do: [:assoc |
				coll add: assoc key asString.
				coll add:  (self dependentsMap at: (self reverseDependentsMap at: assoc value)) _asOop printString].
			stream := WriteStream on: String new.
			coll do: [:each | stream nextPutAll: each; tab ].
			clientUpdateConcentrator update: stream contents ]