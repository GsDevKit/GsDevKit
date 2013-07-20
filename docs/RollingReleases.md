# Rolling Releases

On GitHub, clone the `glass` and `zinc` projects from https://github.com/glassdb.

```Smalltalk
Metacello new
  enableAutoLockHonor
```

## Create local Glass Repository and Load

```Shell
cd /opt/git
git clone https://github.com/<your github account>/glass.git
```

```Smalltalk
Metacello new
  baseline: 'Glass';
  repository: 'filetree:///opt/git/glass/repository';
  get.

Metacello new
  baseline: 'Glass';
  repository: 'filetree:///opt/git/glass/repository';
  onWarning: [:ex | 
    Transcript cr; show: 'WARNING: ' , ex description.
    ex resume ];
  load.

Metacello image
  baseline: 'Glass';
  lock.
```

## Create Local Zinc Repository and Load

```Shell
cd /opt/git
git clone https://github.com/<your github account>/zinc.git
cd zinc
git checkout gemstone2.4 # or gemstone3.1
```

```Smalltalk
Metacello new
  baseline: 'Zinc';
  repository: 'filetree:///opt/git/zinc/repository';
  get.

Metacello new
  baseline: 'Zinc';
  repository: 'filetree:///opt/git/zinc/repository';
  load: 'Tests'.

Metacello image
  baseline: 'Zinc';
  lock.
```

## Fix Issue 135

```Shell
cd /opt/git/glass
git checkout master
git pull origin master
git checkout -b issue_135
```

```
mc commit Squeak.v3 `fix Issue #135: tweak CharacterCollection>>includeSubstring:`
```

```Shell
git commit -a -m"fix Issue #135: tweak CharacterCollection>>includeSubstring:"
git push origin issue_135
```

On GitHub issue a pull request for the *issue_135* branch against the *gemstone2.4* or *gemstone3.1* branch of 
https://github.com/glassdb/glass. This will trigger a https://travis-ci.org build and a committer on the **glassdb** 
team 


