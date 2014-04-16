*bootstrap-accessing
subscriptions
  Subscriptions == nil
    ifTrue: [ 
      Subscriptions := RcIdentityBag new.
      Subscriptions objectSecurityPolicy: nil ].
  ^ Subscriptions