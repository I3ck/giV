giV
===

giV - Semantic versioning for Git repositories



State
-----

v0.1  
`giV` should work properly but is likely going to change in the future (see [TODO](TODO) or issue tracker for problems and missing features)



Build
-----

You'll need [Haskell Stack](https://haskell-lang.org/get-started)  

Clone this repo  
```
$ git clone https://github.com/I3ck/giV.git  
$ cd giV  
```

Either just build `giV`  
```
$ stack upgrade  
$ stack build  
```  

Or install it to your path
```
$ stack upgrade  
$ stack install  
```  



Run
---

`giV` expects `git` in your path  
If you built `giV` via `stack build`, you can run it via  
```
$ stack exec -- giV
```

If you installed it via `stack install`, just type  
```
$ giV
```



### Configuration

Before running, you should edit the [configuration file](giVcfg.yaml) to suit your needs.  
These settings are usually project specific and therefore located within this file.  
Settings that rather change per run can be adjusted via command line args passed to `giV`
```
--help         prints this explanation
--repo         path to the git repository that should be versioned
--cfg          path to the configuration file (explained above)
--branch       the branch that should be versioned, default being master
--outputformat the format of the final output
               [OutputVersion, OutputYAML, OutputJSON]
               OutputVersion being the default
--verbose      print debugging information
```


Output
------

Depending on the `--outputformat` flag, there's several output options.  
The default one just prints the basic version string, while the others provide more information
```
// default
1.2401.0
```
```yaml
# yaml
major: 1
minor: 2401
patch: 0
count: 0
```

```json
// json
{"major":1,"minor":2401,"patch":0,"count":0}
```

Debugging
---------

`giV` with the `--verbose` flag makes it trivial to find issues caused by incorrect configurations.  
The current configuration and both the version changes and resulting version per commit are displayed.  
Below parts of the result of running `giV --verbose` on the Bitcoin repository (`...` -> shortened output)  

```
Fetching...
Parsing...
Processing...
Default change branch: Fix
Default change master: Feature
Major change word: ([mM]ajor|[bB]reaking)
Minor change word: ([mM]inor|[fF]eature)
Patch change word: ([pP]atch|[fF]ix)
No change word: ([nN]o[cC]hange|[nN]one)
Start version: 1.2.3:0
Default change rules:

Feature <- ^[fF]eature
Fix <- ^[rR]elease

MASTER CHANGES
26.59.1:0 -> [FIX]   ("HEAD -> master, origin/master, origin/HEAD") Merge #14385: depends: qt: avoid system harfbuzz and bz2
    f149e31ea2 depends: qt: avoid system harfbuzz and bz2 (Cory Fields)
    ...

26.59.0:0 -> [FEAT]  Merge #13649: test: allow arguments to be forwarded to flake8 in lint-python.sh
    854c85ae90 test: allow arguments to be forwarded to flake8 in lint-python.sh (James O'Beirne)
    ...

26.58.0:0 -> [FEAT]  Merge #14253: Build: during 'make clean', remove some files that are currently missed.
    3f5ac27205 Include some files currently missed by 'make distclean'. (murrayn)
    ...

26.57.2:0 -> [FIX]   Merge #14390: docs: release process: RPC documentation
    3b706212ad doc: RPC documentation (Karel Bílek)
    ...

26.57.1:0 -> [FIX]   Merge #14428: docs: Fix macOS files description in qt/README.md
    0bd64dc6d6 Fix macOS files description (Hennadii Stepanov)
    ...

26.57.0:0 -> [FEAT]  Merge #14324: qa: Run more tests with wallet disabled
    faa4043c66 qa: Run more tests with wallet disabled (MarcoFalke)
    ...
...
```


Examples
--------

```
Legend
------
*Commit / Merge
<Tag]
"Commit Subject"
&calculated version here

Configuration
-------------
majorregexp: BREAKING
patchregexp: FIX
tagversioning: True


State of Repo
-------------


        feature/A  --- * ---  ---
                  /               \
                 /                 \
master --- * --- * --- * --- * --- * --- * --- HEAD
           ^                             ^
           |                             +-- "all changed BREAKING"
           |
           |
           +-- <v1.4.0]




defaultchangemaster: Feature
----------------------------

        feature/A  --- * ---  ---
                  /               \
                 /                 \
master --- * --- * --- * --- * --- * --- * --- HEAD calculated version 2.0.0
           ^     &1.5.0                  ^
           |           &1.6.0            +-- "all changed BREAKING"
           |                 &1.7.0
           |                       &1.8.0
           |                             &2.0.0
           +-- <v1.4.0]




defaultchangemaster: Fix
------------------------

        feature/A  --- * ---  ---
                  /               \
                 /                 \
master --- * --- * --- * --- * --- * --- * --- HEAD calculated version 2.0.0
           ^     &1.4.1                  ^
           |           &1.4.2            +-- "all changed BREAKING"
           |                 &1.4.3
           |                       &1.4.4
           |                             &2.0.0
           +-- <v1.4.0]




defaultchangemaster: Breaking
-----------------------------

        feature/A  --- * ---  ---
                  /               \
                 /                 \
master --- * --- * --- * --- * --- * --- * --- HEAD calculated version 5.0.1
           ^     &2.0.0                  ^
           |           &3.0.0            +-- "some text FIX"
           |                 &4.0.0
           |                       &5.0.0
           |                             &5.0.1
           +-- <v1.4.0]




defaultchangemaster: Breaking
defaultchangebranch: Fix
--branch=feature/A
-----------------------------
                                  &2.0.3
                             &2.0.2
                       &2.0.1
        feature/A  --- * --- * -- *
                  /                \
                 /                  \
master --- * --- * --- * --- * ---  * --- * --- HEAD calculated version 5.0.1
           ^     &2.0.0                   ^
           |           &3.0.0             +-- "some text FIX"
           |                 &4.0.0
           |                       &5.0.0
           |                             &5.0.1
           +-- <v1.4.0]
```


Contribute
----------
If you'd like to contribute, I suggest contacting me beforehand (email / issue tracker), so I can ensure that the Github version is up-to-date.  
If you have any suggestions or would like to report a bug, feel free to do so via the issue tracker


License
-------
See LICENSE
