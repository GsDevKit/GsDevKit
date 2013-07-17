packages
setUpGsDeployerBar5: ancestors
	"self reset"
	
	^self 
		setUpPackageClassDefs: { 
			#Object. #GsDeployerBarA. #('a' 'e'). #('CV'). #('civ').
			#GsDeployerBarA. #GsDeployerBarB. #('b'). #(). #().
			#GsDeployerBarB. #GsDeployerBarC. #('c'). #(). #().
			#GsDeployerBarA. #GsDeployerBarD. #('d'). #(). #().
			} 
		methodDefs: {
			#Object. #deploymentBarExtension. 'deploymentBarExtension ^1'.				"no write permission on class segment"
			#GsDeployerTest. #deploymentBarExtension. 'deploymentBarExtension ^2'.	"write permission on class segment"
			}
		named: 'GsDeployerBar-dkh.5'
		ancestors: ancestors