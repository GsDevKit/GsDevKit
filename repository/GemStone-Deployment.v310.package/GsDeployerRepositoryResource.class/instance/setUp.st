running
setUp
  "self reset"

  | ancestor |
  super setUp.
  self setUpMonticelloRepository.
  ancestor := self setUpGsDeployerBar1.
  ancestor := self setUpGsDeployerBar2: {ancestor}.
  ancestor := self setUpGsDeployerBar3: {ancestor}.
  ancestor := self setUpGsDeployerBar4: {ancestor}.
  ancestor := self setUpGsDeployerBar5: {ancestor}.
  ancestor := self setUpGsDevKitIssue77Bar1.
  ancestor := self setUpGsDevKitIssue77Bar2: {ancestor}.
  ancestor := self setUpGsDevKitIssue77Bar3: {ancestor}.
  ancestor := self setUpGsDevKitIssue83Foo1.
  ancestor := self setUpGsDevKitIssue83Foo2: {ancestor}.
  ancestor := self setUpGsDevKitIssue83Bar1