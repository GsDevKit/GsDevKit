configuration
buildMetagraphOn: root 
	^ self 
		buildMetagraphOn: root
		class: #classes
		comment: #comments
		metaclass: #metaclasses