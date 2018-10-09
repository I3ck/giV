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
  
`giV` will notify you about necessary parameters


Contribute
----------
Since `giV` is currently just a prototype, you should consider contributing later.  
If you still feel like contributing, I suggest contacting me beforehand (email / issue tracker),  
so I can ensure that the Github version is up-to-date.  
If you have any suggestions or would like to report a bug, feel free to do so via the issue tracker


License
-------
See LICENSE
