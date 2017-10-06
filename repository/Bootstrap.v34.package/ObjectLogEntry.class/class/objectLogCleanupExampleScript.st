cleaning
objectLogCleanupExampleScript
  | log date1 date2 count |
  log := self objectLogEntries: true.
  log
    ifNotNil: [ 
      "If it is not nil it means we could get the lock"
      "keep only one per day of various maintenance tasks (useful to go back years on size!)"
      #('MTCE: Repository Size' 'MTCE: MFC' 'MTCE: expired sessions')
        do: [ :eachLabel | 
          log copy
            do: [ :each | 
              each label = eachLabel
                ifTrue: [ 
                  each stamp date = date1
                    ifTrue: [ log remove: each ]
                    ifFalse: [ date1 := each stamp date ] ] ] ].
      "Keep only one day per week of anything more than a month old"
      date1 := Date today subtractDays: 30.
      log copy
        do: [ :each | 
          (each stamp date < date1 and: [ each stamp date day \\ 7 ~= 0 ])
            ifTrue: [ log remove: each ] ].
      "keep only last three days of session expiration"
      date1 := Date today subtractDays: 3.
      log copy
        do: [ :each | 
          (each stamp date < date1 and: [ each label = 'MTCE: expired sessions' ])
            ifTrue: [ log remove: each ] ].
      "keep only five per day of lock failures"
      date1 := nil.
      log copy
        do: [ :each | 
          each stamp date = date1
            ifTrue: [ 
              each label = 'Lock not acquired - retrying'
                ifTrue: [ 
                  count < 5
                    ifTrue: [ count := count + 1 ]
                    ifFalse: [ log remove: each ] ] ]
            ifFalse: [ 
              date1 := each stamp date.
              count := 0 ] ].
      "keep only last 2 days of inspect/explore interactions"
      Smalltalk 
        at: #GsInspectInteraction
        ifPresent: [:cl |
          date1 := Date today subtractDays: 2.
          log copy
            do: [ :each | 
              (each stamp date < date1
                and: [ 
                  (each object isKindOf: cl)
                    or: [ each object isKindOf: cl ] ])
                ifTrue: [ log remove: each ] ] ].
      "beyond ten days keep only twenty errors per day"
      date1 := nil.
      date2 := Date today subtractDays: 10.
      log copy
        do: [ :each | 
          each stamp date = date1
            ifTrue: [ 
              each priority == 2
                ifTrue: [ 
                  count < 20
                    ifTrue: [ count := count + 1 ]
                    ifFalse: [ 
                      date1 < date2
                        ifTrue: [ log remove: each ] ] ] ]
            ifFalse: [ 
              date1 := each stamp date.
              count := 0 ] ] ]
