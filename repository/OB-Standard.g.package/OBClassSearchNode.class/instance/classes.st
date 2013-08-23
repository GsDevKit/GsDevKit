navigation
classes
	^ ((SystemNavigation default allClasses select: [ :ea | self classSatisfiesQuery: ea ]) collect: [ :ea | OBClassNode on: ea ]) asArray