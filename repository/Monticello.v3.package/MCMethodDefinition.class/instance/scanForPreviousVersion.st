installing
scanForPreviousVersion
	"answer the newest pevious version not in this categorie, ie the original version"
	| versions histVersions |
	versions := OrderedCollection new.
	histVersions := 	MethodVersionHistory uniqueInstance 
		versionsOfMethod: self selector 
		in: self actualClass.
	^ histVersions detect: [:record |
		record category ~= category].
