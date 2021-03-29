private
cleanUndefinedSymbols
	| removals |
	removals := Set new.
	UndefinedSymbols keysAndValuesDo: [:key :value |
		(value isKindOf: Set)
			ifTrue: [
				value copy do: [:assoc | | class selector remove |
					remove := true.
					(assoc isKindOf: Association)
						ifTrue: [
							class := assoc key.
							selector := assoc value.
							(Smalltalk hasClassNamed: class theNonMetaClass name)
								ifTrue: [ | currentClass |
									currentClass := Smalltalk at: class theNonMetaClass name asSymbol.
									class isMeta ifTrue: [ currentClass := currentClass class ].
									(currentClass compiledMethodAt: selector ifAbsent: [nil]) ~~ nil
										ifTrue: [
											[ currentClass recompile: selector ] 
												on: UndefinedSymbolNotification
												do: [:ex | remove := false ]]]].
					remove ifTrue: [ value remove: assoc ]].
				value isEmpty ifTrue: [ removals add: key] ]].
	removals do: [:key | UndefinedSymbols removeKey: key ifAbsent: [ nil ]].