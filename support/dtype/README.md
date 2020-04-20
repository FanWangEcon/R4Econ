# Naming Conventions

Try to use the same naming convention across languages, fan varible naming Conventions:

To help clarify data types for inputs, add prefix to variable names:

## Strings

Various string prefixes:

**Time**

- *tm*: time variables values etc that can used for calculations

**Dataset Variable Names**

- *svr*: variable string, as in data variable name
- *slb*: string variables labels etc
-
**Function Names and Path**

- *spt*: string path, path only, does not include name
- *spn*: string path, including name
- *sfc*: function string name

**String Code**

- *spg* = not string, but code, to be interpreted as code later

**Graph String**

- *stg*

- oj = some kind of generic anything object
- fl = float
- it = integer
- ar = single dimensional array
  + ar.st
  + ar.fl
  + ar_it
- mt = matrix
- ts = higher dimensions matrix, 3d tensor
- ls = list
  + list.ar.st: list of arrays of strings
  + ls_svr: list of string variable namess
- sc = structure, generic large store
- df = dataframe
  + tb if tb, but tb can also be called df.
- fc = functions

Regressions

- rs: reg results
- rsm: result summary
- vf: factor variable

acronyms

- drv: discrete random variable
- smp: sample

files

- fst: fan_script_test

function naming

- ff: generic
- ffi: inner small local function
