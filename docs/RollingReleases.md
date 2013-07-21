# Rolling Releases

## Local Glass Repository

On GitHub, fork the **glass** project from
https://github.com/glassdb/glass.

### Create a clone of your repository to local disk:

```Shell
cd /opt/git
git clone https://github.com/<your github account>/glass.git
cd glass
git remote add glassdb https://github.com/glassdb/glass.git
```

### Load **Glass**:

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

### Lock the Glass project:

```Smalltalk
Metacello new
  baseline: 'Glass';
  repository: 'filetree:///opt/git/glass/repository';
  autoHonor;
  lock.
```

By locking the project and using the `autoHonor` option, all references
to the **Glass** project will be satisfied using the repository in `/opt/git/glass/repository`.

## Local Zinc Repository

On GitHub, fork the `zinc` project from https://github.com/glassdb/zinc.

```Shell
cd /opt/git
git clone https://github.com/<your github account>/zinc.git
cd zinc
git remote add glassdb https://github.com/glassdb/zinc.git
git checkout gemstone2.4 # or gemstone3.1
```

### Load and lock Zinc:

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

## Contribute to Rolling Glass Release

### Create issue branch:

```Shell
cd /opt/git/glass
git checkout master
git pull origin master
git checkout -b issue_XXX
```

### Push issue branch to GitHub when done:

```Shell
git push origin issue_XXX
```

On GitHub issue a 
pull request for the *issue branch*. 
This will trigger a https://travis-ci.org build and a committer on the **glassdb** 
team will review the change and merge pull request.

## Update from Rolling Glass Release

When you are ready to update to the latest Rolling Glass Release you
need to pull the head of https://github.com/glassdb/glass to your
local repository by using the remote *glassdb* repository created
earlier:

### Fetch and Pull from glassdb repository:

```Shell
cd /opt/git/glass
git fetch glassdb master
git pull glassdb master
```

### Load latest code into image:

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

