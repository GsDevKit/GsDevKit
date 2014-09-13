"Load and run tests to be performed by TravisCI"
| gitCache gitPath |
Transcript cr; show: 'travis---->travisCI.st'.

(MCWorkingCopy allManagers 
  collect: [ :wc | wc ancestors first name ]) sorted 
    do: [:packageName |
      Transcript cr; show: packageName].

gitCache := 'git_cache'.
gitPath := (FileDirectory default directoryNamed: gitCache ) fullName.

GsDeployer deploy: [
  "Load the GLASS1 baseline"
  Metacello new
    baseline: 'GLASS1';
    repository: 'filetree://', gitPath, '/glass/repository';
    get.
  Metacello new
    baseline: 'GLASS1';
    repository: 'filetree://', gitPath, '/glass/repository';
    onConflict: [ :ex | ex allow ];
    onWarning: [ :ex | 
        Transcript
          cr;
          show: ex description.
        ex resume ];
    load: #( 'default') ].

"Run the GLASS1 tests"
MetacelloPlatform current authorName: 'testMonkey'. "These tests save code ... need an Author"

Transcript cr; show: '---Start running tests...'.

[
TravisCIHarness
  value: #( 'BaselineOfGLASS1' )
  value: 'TravisCISuccess.txt' 
  value: 'TravisCIFailure.txt'.
]
        on: Deprecation
        do: [ :ex |
            "catch and throw Error ..."
            true ifTrue: [nil error: 'DEPRECATION WARNNG: ', ex description printString ].
            "help for debugging Deprecation warnings"
            Transcript
                cr;
                show: '***Deprecation***************************************************';
                cr;
                show: ex description.
            (ex signalerContext stackOfSize: 100)
                do: [ :item | 
                    Transcript
                        cr;
                        show: item printString asString.
                    item tempScopedNames
                        doWithIndex: [ :title :index | 
                            Transcript
                                cr;
                                tab;
                                nextPutAll: title;
                                nextPut: $:;
                                space;
                                tab;
                                nextPutAll: (item tempAt: index) printString ].
                    Transcript
                        cr;
                        show: '***************************************************' ].
            ex resume].

