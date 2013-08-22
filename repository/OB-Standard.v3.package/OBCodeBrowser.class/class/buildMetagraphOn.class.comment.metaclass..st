configuration
buildMetagraphOn: root class: classSel comment: commentSel metaclass: metaclassSel 
	^ (OBMetagraphBuilder
		on: root
		class: classSel
		comment: commentSel
		metaclass: metaclassSel) execute