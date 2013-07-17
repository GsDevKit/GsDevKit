running
setUp
	"self reset"

	| ancestor |
	super setUp.
	self setUpMonticelloRepository.
	ancestor := self setUpGsDeployerBar1.
	ancestor := self setUpGsDeployerBar2: { ancestor }.
	ancestor := self setUpGsDeployerBar3: { ancestor }.
	ancestor := self setUpGsDeployerBar4: { ancestor }.
	ancestor := self setUpGsDeployerBar5: { ancestor }.
