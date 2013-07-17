accessing
bulkMigrate
	"turn off autoMigrate during block evaluation and do bulk migrate after block evaluated"

	bulkMigrate ifNil: [ bulkMigrate := false ].
	^bulkMigrate