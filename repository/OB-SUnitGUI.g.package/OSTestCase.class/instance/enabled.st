accessing
enabled
	"Enabled := true"
	"Enabled := nil"

	Enabled == nil ifTrue: [ ^false ].
	^Enabled