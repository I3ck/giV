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
$ stack build  
```  
  
Or install it to your path
```
$ stack install  
```  


Run
---
`giV` expects `git` in your path.  
If you built `giV` via `stack build`, you can run it via  
```
$ stack exec -- giV
```
  
If you installed it via `stack install`, just type  
```
$ giV
```
  
`giV --help` will notify you about the usage  


Example
-------

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
