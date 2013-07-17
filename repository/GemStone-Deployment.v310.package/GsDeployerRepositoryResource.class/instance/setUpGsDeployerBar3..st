packages
setUpGsDeployerBar3: ancestors
	"self reset"
	
	^self 
		setUpPackageClassDefs: { 
			#Object. #GsDeployerBarA. #('a' 'e'). #('CV'). #('civ').
			#GsDeployerBarA. #GsDeployerBarB. #('b'). #(). #().
			#GsDeployerBarB. #GsDeployerBarC. #('c' 'f'). #(). #().
			#GsDeployerBarA. #GsDeployerBarD. #('d' 'f'). #(). #().
			} 
		named: 'GsDeployerBar-dkh.3'
		ancestors: ancestors