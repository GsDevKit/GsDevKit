# Rolling Releases

## Fork Glass Repository

On GitHub, fork the **glass** project from
https://github.com/glassdb/glass.

Create a clone of your repository to local disk:

```Shell
cd /opt/git
git clone https://github.com/<your github account>/glass.git
```

Load **Glass** from your local repository:

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
```

Now lock the **Glass** project:

```Smalltalk
Metacello image
  baseline: 'Glass';
  autoHonor;
  lock.
```

By locking the project and using the `autoHonor` option, all references
to the **Glass** project will be satisfied using 

## Create Local Zinc Repository and Load

On GitHub, fork the `zinc` project from https://github.com/glassdb/zinc.

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
  autoHonor;
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
team will review the change and merge pull request when everything is
satisfactory.

## Merge Bugfix from glassdb Repository

```Shell
cd /opt/git/glass
git checkout master
git pull origin master
git remote add glass https://github.com/glassdb/glass.git
git fetch glass master
git checkout -b dev
git merge glass master
```

Load baseline and latest packages from `dev` branch:

```Smalltalk
Metacello new
  baseline: 'Glass';
  repository: 'filetree:///opt/git/glass/repository';
  get.

Metacello new
  baseline: 'Glass';
  repository: 'filetree:///opt/git/glass/repository';
  load.
```

Test merged **Glass** project in your environment:

```Shell
test project Glass
``

Merge `dev` branch into your `master` branch:

```Shell
cd /opt/git/glass
git checkout master
git merge --no-ff dev
git push origin master
```

