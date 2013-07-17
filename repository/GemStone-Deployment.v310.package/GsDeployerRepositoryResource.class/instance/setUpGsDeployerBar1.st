packages
setUpGsDeployerBar1
	"self reset"
	
	^self 
		setUpPackageClassDefs: { 
			#Object. #GsDeployerBarA. #('a'). #('CV'). #('civ').
			#GsDeployerBarA. #GsDeployerBarB. #('b'). #(). #().
			#GsDeployerBarB. #GsDeployerBarC. #('c'). #(). #().
			#GsDeployerBarA. #GsDeployerBarD. #('d'). #(). #().
			} 
		named: 'GsDeployerBar-dkh.1'
		ancestors: #()