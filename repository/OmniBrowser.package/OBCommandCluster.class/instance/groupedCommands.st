accessing
groupedCommands
	| groups |
	groups := commands 
				groupBy: [:svc | svc group]
				having: [:group | group isEmpty not].
	groups := groups collect: [:ea | ea asArray sort: [:a :b | a label <= b label]].
	^ groups asArray sort: [:a :b | a anyOne group <= b anyOne group]