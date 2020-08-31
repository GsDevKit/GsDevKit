as yet unclassified
associate: tokens
	| result |
	result _ Dictionary new.
	tokens pairsDo: [:key :value | | v |
                    v := value.
					v isString ifFalse: [v _ v collect: [:ea | self associate: ea]].
					v = 'nil' ifTrue: [v _ ''].
					result at: key put: v].
	^ result