callbacks
keystroke: aChar from: aMorph
	| scan |
	self isEmpty ifTrue: [^ self].	
	scan _ self announcer announce: OBNodeCommandScan.
	scan processKeystroke: aChar withNode: self parent for: self.
	self hasSelection ifTrue: [scan 
								processKeystroke: aChar 
								withNode: self selectedNode 
								for: self].