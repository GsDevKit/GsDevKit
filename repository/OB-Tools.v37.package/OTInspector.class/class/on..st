instance-creation
on: anObject
	| browser |
	browser := self root: (OTRootInspectorNode on: anObject).
	browser jumpTo: browser root childNodes first.
	^ browser