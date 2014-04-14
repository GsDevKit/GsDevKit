baseline
baseline: spec
  <baseline>
  spec
    for: #'gemstone'
    do: [ 
      spec
        configuration: 'GoferProjectLoader'
          with: [ spec repository: 'http://seaside.gemstone.com/ss/MetacelloRepository' ];
        configuration: 'Grease'
          with: [ 
              spec
                versionString: '1.0.7.2';
                loads: #('Core');
                repository: 'http://www.squeaksource.com/MetacelloRepository' ];
        yourself.
      spec
        package: 'GemStone-Interactions';
        package: 'Core' with: [ spec requires: #('GemStone-Interactions') ];
        package: 'Base-Bootstrap' with: [ spec requires: #('Core') ];
        package: 'Bootstrap' with: [ spec requires: #('Base-Bootstrap') ];
        package: 'GemStone-ANSI-Streams' with: [ spec requires: #('Core') ];
        package: 'GemStone-Indexing-Extensions'
          with: [ spec requires: #('Bootstrap') ];
        package: 'GemStone-Release-Support'
          with: [ spec requires: #('GsCore') ];
        package: 'Sport'
          with: [ 
              spec
                file: 'Sport3.010-dkh.28';
                requires: #('Bootstrap');
                repository: 'http://seaside.gemtalksystems.com/ss/hyper' ];
        package: 'Squeak' with: [ spec requires: #('Sport') ];
        package: 'Regex-Core' with: [ spec requires: #('Core' 'Squeak') ];
        package: 'Regex-Tests-Core' with: [ spec requires: #('Regex-Core') ];
        yourself.
      spec
        package: 'Announcements' with: [ spec requires: #('GsCore') ];
        package: 'SmaCC' with: [ spec requires: #('GsCore') ];
        package: 'SMTPMail' with: [ spec requires: #('GsCore') ];
        package: 'System-Digital-Signatures'
          with: [ spec requires: #('GsCore') ];
        package: 'Utf8Encoding' with: [ spec requires: #('GsCore') ];
        yourself.
      spec
        package: 'Change-Notification' with: [ spec requires: #('Monticello') ];
        package: 'PackageInfo-Base'
          with: [ 
              spec
                file: 'PackageInfo-Base.g';
                requires: #('GsCore') ];
        package: 'Monticello' with: [ spec requires: #('PackageInfo-Base') ];
        package: 'GemStone-Deployment'
          with: [ 
              spec
                postLoadDoIt: #'deploymentPostLoadDoIt';
                requires: #('Monticello') ];
        yourself.
      spec
        package: 'OmniBrowser'
          with: [ spec requires: #('Core' 'GemStone-Interactions') ];
        package: 'OB-GemStone-Platform'
          with: [ spec requires: #('OmniBrowser') ];
        package: 'OB-Monticello'
          with: [ spec requires: #('OB-Standard' 'Monticello') ];
        package: 'OB-SUnitIntegration'
          with: [ spec requires: #('OB-Standard') ];
        package: 'OB-SUnitGUI'
          with: [ 
              spec
                file: 'OB-SUnitGUI.g';
                requires: #('OB-Standard') ];
        package: 'OB-Standard'
          with: [ spec requires: #('OB-GemStone-Platform' 'Change-Notification') ];
        package: 'OB-Tools'
          with: [ spec requires: #('OB-Standard' 'Announcements') ];
        package: 'JadeServer' with: [ spec requires: #('OB-Tools') ];
        yourself.
      spec
        package: 'GsSqueakCommon-Core'
          with: [ spec requires: #('GsCore' 'Grease') ];
        package: 'GsSqueakCommon-Tests'
          with: [ spec requires: #('GsSqueakCommon-Core') ];
        yourself.
      spec
        group: 'default' with: #('GLASS');
        group: 'GLASS'
          with:
            #('GsCore' 'GsMonticello' 'GoferProjectLoader' 'GsOB' 'Grease' 'GsSqueakCommon');
        group: 'GsCore'
          with:
            #('Core' 'Base-Bootstrap' 'Bootstrap' 'GemStone-ANSI-Streams' 'GemStone-Indexing-Extensions' 'Sport' 'Squeak' 'Regex-Core' 'Regex-Tests-Core' 'GemStone-Release-Support');
        group: 'GsMisc'
          with:
            #('Announcements' 'SmaCC' 'SMTPMail' 'System-Digital-Signatures' 'Utf8Encoding' 'XML-Parser');
        group: 'GsMonticello'
          with:
            #('Change-Notification' 'PackageInfo-Base' 'Monticello' 'GemStone-Deployment');
        group: 'GsOB'
          with:
            #('OmniBrowser' 'OB-GemStone-Platform' 'OB-Monticello' 'OB-SUnitIntegration' 'OB-SUnitGUI' 'OB-Standard' 'OB-Tools' 'JadeServer');
        group: 'GsSqueakCommon'
          with: #('GsSqueakCommon-Core' 'GsSqueakCommon-Tests');
        yourself ].
  spec
    for: #'gs2.x'
    do: [ 
      spec
        package: 'Announcements' with: [ spec file: 'Announcements.g' ];
        package: 'GemStone-Exceptions'
          with: [ spec requires: #('Bootstrap' 'Sport') ];
        package: 'GemStone-Release-Support'
          with: [ spec file: 'GemStone-Release-Support.v2x' ];
        package: 'GsRandom' with: [ spec requires: #('Base-Bootstrap') ];
        package: 'GsSqueakCommon-Core'
          with: [ spec file: 'GsSqueakCommon-Core.2x' ];
        package: 'OB-Standard' with: [ spec file: 'OB-Standard.g' ];
        package: 'OB-Tools' with: [ spec file: 'OB-Tools.g' ];
        package: 'Sport' with: [ spec requires: #('GsRandom') ];
        package: 'Utf8Encoding' with: [ spec file: 'Utf8Encoding.230' ];
        yourself.
      spec
        package: 'Monticello' with: [ spec file: 'Monticello.g' ];
        yourself.
      spec
        group: 'GsCore' with: #('GsRandom' 'GemStone-Exceptions');
        yourself ].
  spec
    for: #'gs2.4.x'
    do: [ 
      spec
        configuration: 'XMLSupport'
          with: [ 
              spec
                versionString: '1.0.5';
                loads: #('Core');
                repository:
                    'http://seaside.gemtalksystems.com/ss/MetacelloRepository' ];
        yourself.
      spec group: 'XML-Parser' with: #('XMLSupport') ].
  spec
    for: #'Issue29FixANeeded'
    do: [ 
      spec
        package: 'GemStone-244-Exceptions'
        with: [ spec requires: #('GemStone-Exceptions') ].
      spec
        group: 'GsCore' with: #('GemStone-244-Exceptions');
        yourself ].
  spec
    for: #'Issue29FixBNeeded'
    do: [ 
      spec
        package: 'GemStone-245-Exceptions'
        with: [ spec requires: #('GemStone-Exceptions') ].
      spec
        group: 'GsCore' with: #('GemStone-245-Exceptions');
        yourself ].
  spec
    for: #'gs3.x'
    do: [ 
      spec
        configuration: 'XMLSupport'
          with: [ 
              spec
                versionString: '1.0.5';
                loads: #('Core');
                repository:
                    'http://seaside.gemtalksystems.com/ss/MetacelloRepository' ];
        yourself.
      spec group: 'XML-Parser' with: #('XMLSupport').
      spec
        package: 'Core' with: [ spec file: 'Core.v3' ];
        package: 'Base-Bootstrap' with: [ spec file: 'Base-Bootstrap.v3' ];
        package: 'Bootstrap' with: [ spec file: 'Bootstrap.v3' ];
        package: 'GsSqueakCommon-Core'
          with: [ spec file: 'GsSqueakCommon-Core.3x' ];
        package: 'OB-Standard' with: [ spec file: 'OB-Standard.v3' ];
        package: 'OB-SymbolListBrowser'
          with: [ spec requires: #('OB-Standard') ];
        package: 'OB-Tools' with: [ spec file: 'OB-Tools.v3' ];
        package: 'Sport' with: [ spec file: 'Sport3.010.v3-dkh.30' ];
        package: 'Squeak' with: [ spec file: 'Squeak.v3' ];
        yourself.
      spec
        package: 'Change-Notification'
          with: [ spec file: 'Change-Notification.v310' ];
        package: 'GemStone-Deployment'
          with: [ spec file: 'GemStone-Deployment.v310' ];
        package: 'Monticello' with: [ spec file: 'Monticello.v3' ];
        yourself.
      spec
        group: 'GsOB' with: #('OB-SymbolListBrowser');
        yourself ].
  spec
    for: #'gs3.0.x'
    do: [ 
      spec
        package: 'Change-Notification'
          with: [ spec file: 'Change-Notification.v300' ];
        package: 'GemStone-Deployment'
          with: [ spec file: 'GemStone-Deployment' ];
        package: 'Monticello' with: 'Monticello.v301';
        package: 'Utf8Encoding' with: [ spec file: 'Utf8Encoding.230' ];
        yourself ].
  spec
    for: #'gs3.1.x'
    do: [ 
      spec
        package: 'Utf8Encoding'
        with: [ 
          spec
            file: 'Utf8Encoding.310';
            requires: #('GsCore') ] ].
  spec
    for: #'gs3.2.x'
    do: [ 
      spec
        package: 'Squeak' with: [ spec file: 'Squeak.v32' ];
        package: 'Utf8Encoding'
          with: [ 
              spec
                file: 'Utf8Encoding.310';
                requires: #('GsCore') ] ].
  spec
    for: #'maglev'
    do: [ spec package: 'Bootstrap' with: [ spec file: 'Bootstrap.maglev' ] ].
  spec
    for: #'Issue3TraitLoadFixNeeded'
    do: [ 
      spec
        package: 'Monticello'
        with: [ spec preLoadDoIt: #'traitsMonticelloPreLoadDoIt' ] ]