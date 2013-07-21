# Rolling Releases

## Glass Repository

On GitHub, fork the **glass** project from
https://github.com/glassdb/glass.

Create a clone of your repository to local disk:

```Shell
cd /opt/git
git clone https://github.com/<your github account>/glass.git
cd glass
git remote add glass https://github.com/glassdb/glass.git
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
Metacello new
  baseline: 'Glass';
  repository: 'filetree:///opt/git/glass/repository';
  autoHonor;
  lock.
```

By locking the project and using the `autoHonor` option, all references
to the **Glass** project will be satisfied using the repository in `/opt/git/glass/repository`.

## Zinc Repository

On GitHub, fork the `zinc` project from https://github.com/glassdb/zinc.

```Shell
cd /opt/git
git clone https://github.com/<your github account>/zinc.git
cd zinc
git remote add glassdb https://github.com/glassdb/zinc.git
git checkout gemstone2.4 # or gemstone3.1
```

Load **Zinc** from local repository:

```Smalltalk
Metacello new
  baseline: 'Zinc';
  repository: 'filetree:///opt/git/zinc/repository';
  get.

Metacello new
  baseline: 'Zinc';
  repository: 'filetree:///opt/git/zinc/repository';
  load: 'Tests'.
```

Now lock the **Zinc** project:

```Smalltalk

Metacello image
  baseline: 'Zinc';
  autoHonor;
  lock.
```

## Contribute to Rolling Glass Release

To fix a bug, you should create a *feature branch* so that the bugfix
fix can be easily shared with the community:

```Shell
cd /opt/git/glass
git checkout master
git pull origin master
git checkout -b issue_XXX
```

Fix the bug in your image (don't forget to add tests) and commit your
work (tODE command) from Smalltalk to disk:

```
mc commit Squeak.v3 `fix Issue #XXX`
```

Do a git commit and push your work to GitHub:

```Shell
git commit -a -m"fix Issue #XXX"
git push origin issue_XXX
```

On GitHub issue a pull request for the *issue_XXX* branch against the *gemstone2.4* or *gemstone3.1* branch of 
https://github.com/glassdb/glass. This will trigger a https://travis-ci.org build and a committer on the **glassdb** 
team will review the change and merge pull request when everything is
satisfactory.

## Update from Rolling Glass Release

When you are ready to update to the latest Rolling Glass Release
```Shell
cd /opt/git/glass
git checkout -b dev
git fetch glassdb master
git pull glassdb master
```

Load baseline and latest packages from `dev` branch into your image:

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

Test merged **Glass** project:

```Shell
test project Glass
test project <your tests>
``

Merge `dev` branch into your `master` branch:

```Shell
cd /opt/git/glass
git checkout master
git merge --no-ff dev
git push origin master
```

