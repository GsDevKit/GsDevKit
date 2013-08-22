execution
execute
	| refs ar |
	OBPlatform current doAutoCommit
		ifFalse: [OBInformRequest message: 'Auto commit failed. You will need to abort and try again.'].
	System _generationScavenge_vmMarkSweep.
	ar := Array with: target value.
	refs := SystemRepository listReferences: ar.
	refs := refs at: 1.
	refs remove: ar ifAbsent: [].
	refs := refs reject: [:each | each isKindOf: OTInspectorNode ].
	refs explore