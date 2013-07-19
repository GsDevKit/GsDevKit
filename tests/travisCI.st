"Load and run tests to be performed by TravisCI"
| gitCache gitPath |
Transcript cr; show: 'travis---->travisCI.st'.

(MCWorkingCopy allManagers 
  collect: [ :wc | wc ancestors first name ]) sorted 
    do: [:packageName |
      Transcript cr; show: packageName].

gitCache := 'git_cache'.
gitPath := (FileDirectory default directoryNamed: gitCache ) fullName.

[
"Load the bleeding edge for Metacello...at least for now I want to pick up GLASS-specific bugfixes"
Metacello new
  baseline: 'Metacello';
  repository: 'github://dalehenrich/metacello-work:master/repository';
  get.
Metacello new
  baseline: 'Metacello';
  repository: 'github://dalehenrich/metacello-work:master/repository';
  onConflict: [:ex | ex allow];
  load.

(MCWorkingCopy allManagers 
  collect: [ :wc | wc ancestors first name ]) sorted 
    do: [:packageName |
      Transcript cr; show: packageName].

"Load the GLASS1 baseline"
Metacello new
	baseline: 'GLASS1';
  repository: 'filetree://', gitPath, '/glass/repository';
  get.

Metacello new
	baseline: 'GLASS1';
  repository: 'filetree://', gitPath, '/glass/repository';
  load: #( 'GLASS').
] on: Warning do: [:ex | 
  Transcript cr; show: 'WARNING: ', ex description.
  ex resume: true ].

(MCWorkingCopy allManagers 
  collect: [ :wc | wc ancestors first name ]) sorted 
    do: [:packageName |
      Transcript cr; show: packageName].

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

