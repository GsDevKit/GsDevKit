as yet unclassified
classes
	| nav classes |
	nav := self systemNavigation.
	classes := IdentitySet new.
	(Symbol selectorsContaining: query) 
		do: [:ea | classes addAll: (nav allClassesImplementing: ea)].
	^ classes asArray collect: [:ea | OBClassNode on: ea]