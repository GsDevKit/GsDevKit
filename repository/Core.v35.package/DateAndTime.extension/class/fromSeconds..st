*core-squeak
fromSeconds: seconds
  "Answer a DateAndTime since the Squeak epoch: 1 January 1901"

  ^self epoch + (Duration seconds: seconds)