giV
===

giV - Semantic versioning for Git repositories



State
-----

Currently under development, do not expect the behaviour of this program being stable



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
--help    prints this explanation
--repo    path to the git repository that should be versioned
--cfg     path to the configuration file (explained above)
--branch  the branch that should be versioned, default being master
--verbose output debugging information
```


Debugging
---------

`giV` with the `--verbose` flag makes it trivial to find issues caused by incorrect configurations.  
The current configuration and both the version changes and resulting version per commit are displayed.  
Below parts of the result of running `giV --verbose` on the Bitcoin repository  
  
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
Default change rules: 

Feature <- ^[fF]eature
Fix <- ^[rR]elease

1.2401.0 -> [FEAT ] Merge #14385: depends: qt: avoid system harfbuzz and bz2 ["HEAD -> master, origin/master, origin/HEAD"]
1.2400.0 -> [FEAT ] Merge #13649: test: allow arguments to be forwarded to flake8 in lint-python.sh
1.2399.0 -> [FEAT ] Merge #14253: Build: during 'make clean', remove some files that are currently missed.
1.2398.0 -> [FEAT ] Merge #14390: docs: release process: RPC documentation
1.2397.1 -> [ FIX ] Merge #14428: docs: Fix macOS files description in qt/README.md
1.2397.0 -> [FEAT ] Merge #14324: qa: Run more tests with wallet disabled
1.2396.0 -> [FEAT ] Merge #14413: tests: Allow closed rpc handler in assert_start_raises_init_error
1.2395.0 -> [FEAT ] Merge #14241: appveyor: script improvement
...
```


Examples
--------

Calculating the version of `master` with `BREAKING` defined as keyword for major increments, `FIX` as keyword for patch increments

```
*Commit / Merge

<Tag]

"Commit Subject"

&calculated version




State of Branch
---------------


        feature/A  --- * ---  ---
                  /               \
                 /                 \
master --- * --- * --- * --- * --- * --- * --- HEAD
           ^                             ^
           |                             +-- "all changed BREAKING"
           |
           |
           +-- <v1.4.0]




Default: Minor
----------------

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




Default: Patch
----------------

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




Default: Major
----------------

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
```


Contribute
----------
Since `giV` is currently just a prototype, you should consider contributing later.  
If you still feel like contributing, I suggest contacting me beforehand (email / issue tracker),  
so I can ensure that the Github version is up-to-date.  
If you have any suggestions or would like to report a bug, feel free to do so via the issue tracker


License
-------
See LICENSE
