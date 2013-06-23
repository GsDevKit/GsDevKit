*bootstrap-accessing
subscriptions

        Subscriptions == nil 
          ifTrue: [ 
              Subscriptions := RcIdentityBag new.
              Subscriptions changeToSegment: nil ].
        ^Subscriptions
