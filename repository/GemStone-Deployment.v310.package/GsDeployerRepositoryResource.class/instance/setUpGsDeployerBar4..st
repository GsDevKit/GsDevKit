packages
setUpGsDeployerBar4: ancestors
	"self reset"
	
	^self 
		setUpPackageClassDefs: { 
			#Object. #GsDeployerBarA. #('a' 'e' 'f'). #('CV'). #('civ').
			#GsDeployerBarA. #GsDeployerBarB. #('b'). #(). #().
			#GsDeployerBarB. #GsDeployerBarC. #('c'). #(). #().
			#GsDeployerBarA. #GsDeployerBarD. #('d'). #(). #().
			} 
		named: 'GsDeployerBar-dkh.4'
		ancestors: ancestors