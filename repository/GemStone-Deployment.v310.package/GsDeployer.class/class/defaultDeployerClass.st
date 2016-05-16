defaults
defaultDeployerClass
  DefaultClassBinding ifNil: [ ^ self ].
  ^ DefaultClassBinding value