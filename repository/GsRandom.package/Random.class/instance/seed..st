Initialization
seed: aSeed
  (aSeed <= 0 or:[aSeed >= 2147483646]) ifTrue:[ 
     self error:'seed out of range'
  ].
  seed := aSeed.
  hi := lo := nil.