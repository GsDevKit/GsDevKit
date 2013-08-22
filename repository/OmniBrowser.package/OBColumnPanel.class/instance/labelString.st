accessing
labelString
	| label |
	self columns reversed do: 
		[:ea | 
		label := ea selectedNode ifNotNilDo: [:node | node title].
		label ifNotNil: [^ label]].
	^ nil