*monticello
migrateInstances: instances to: anotherClass
	"Migrates the objects in <instances>, which must be instances of the receiver,
 to anotherClass, using migrateFrom:instVarMap: and performing become: operations
 to accomplish this task.  Removes the indexes of indexed instances.
 Returns an Array of five Sets of instances:

 1. Objects that you cannot read.
 2. Objects that you cannot write.
 3. Objects that are in indexed collections that have different formats.
   (For a more detailed description, see Object | become:.)
 4. Objects whose class is not identical to the receiver.
 5. Objects that could not be migrated due to a MigrationError.

 Objects in any of these sets were not migrated."

	| checkidx aMap outerInst enumInIndex enumWrite enumRead enumMigrate errSyms noread nowrite inIndex badClass migrationErrs |
	(self isMeta or: [ anotherClass isMeta ])
		ifTrue: [ ArgumentError signal: 'cannot migrate meta classes' ].
	enumInIndex := (errSyms := ErrorSymbols) at: #'rtErrCantBecomeOneIdx'.
	enumRead := errSyms at: #'authErrSegRead'.
	enumWrite := errSyms at: #'authErrSegWrite'.
	noread := IdentitySet new.
	nowrite := IdentitySet new.
	inIndex := IdentitySet new.
	badClass := IdentitySet new.
	migrationErrs := IdentitySet new.
	enumMigrate := MigrationError errorNumber.
	checkidx := self inheritsFrom: UnorderedCollection.
	aMap := InstVarMappingArray mappingFrom: self to: anotherClass.
	1 to: instances size do: [ :i | 
		[ 
		| inst |
		inst := instances _at: i.
		outerInst := inst.
		(checkidx and: [ inst _hasIndexes ])
			ifTrue: [ inst removeAllIndexes ].
		inst class == self
			ifFalse: [ badClass add: inst ]
			ifTrue: [ 
				| invariant sema other |
				invariant := inst isInvariant.
				other := anotherClass migrateNew.
				other migrateFrom: inst instVarMap: aMap.
				invariant
					ifTrue: [ other immediateInvariant ].
				((inst _onStackAsSelfBelow: 1) or: [ other _onStackAsSelfBelow: 1 ])
					ifTrue: [ 
						sema := Semaphore new.
						[ 
						other become: inst.
						sema signal ] fork.
						sema wait ]
					ifFalse: [ other become: inst ] ] ]
			onSynchronous: Exception
			do: [ :ex | 
				| num |
				num := ex number.
				num == enumInIndex
					ifTrue: [ 
						inIndex add: outerInst.
						ex return ]
					ifFalse: [ 
						num == enumRead
							ifTrue: [ 
								noread add: outerInst.
								ex return ]
							ifFalse: [ 
								num == enumWrite
									ifTrue: [ 
										nowrite add: outerInst.
										ex return ]
									ifFalse: [ 
										num == enumMigrate
											ifTrue: [ 
												migrationErrs add: outerInst.
												ex return ] ] ] ].
				ex pass ] ].
	^ {noread.
	nowrite.
	inIndex.
	badClass.
	migrationErrs}