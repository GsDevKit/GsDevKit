doits
deploymentPostLoadDoIt
	"reset test resources"
	
	#( 
		GsDeployerRepositoryResource
	) do: [:className | Smalltalk at: className ifPresent: [:cls | cls reset ]]