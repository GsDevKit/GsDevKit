updating
okToReclaimPane
	columns size > minPanes ifFalse: [^ false].
	^ columns last isEmpty or: [(columns at: columns size - 1) shouldBeLast].