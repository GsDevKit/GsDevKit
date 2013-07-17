packages
setUpGsDeployerBar2: ancestors
	"self reset"
	
	^self 
		setUpPackageClassDefs: { 
			#Object. #GsDeployerBarA. #('a' 'e'). #('CV'). #('civ').
			#GsDeployerBarA. #GsDeployerBarB. #('b'). #(). #().
			#GsDeployerBarB. #GsDeployerBarC. #('c'). #(). #().
			#GsDeployerBarA. #GsDeployerBarD. #('d'). #(). #().
			} 
		named: 'GsDeployerBar-dkh.2'
		ancestors: ancestors