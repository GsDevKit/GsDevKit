doits
deploymentPostLoadDoIt
	"reset test resources"
	
	#( 
		'GsDeployerRepositoryResource'
	) do: [:className |
          (GsSession currentSession symbolList objectNamed: className) 
            ifNotNil: [:cls | cls reset ]]
