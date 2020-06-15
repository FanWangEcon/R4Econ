# Naming Conventions

Try to use the same naming convention across languages, fan varible naming Conventions:

To help clarify data types for inputs, add prefix to variable names:

## Strings

Various string prefixes:

### Strings

Main String

- *st*: main string prefix, for lines
- *stf*: prefix for entire string files, all lines, including read in and to be reading in.

**Time**

- *tm*: time variables values etc that can used for calculations

**Dataset Variable Names**

- *svr*: variable string, as in data variable name
- *slb*: string variables labels etc

**File Names and Path**

- *spt*: absolute string path, path only, does not include name.
- *spn*: absolute string path, including name.

- *srt*: relative file path, no name
- *srn*: relative file path, with name

- *snm*: function name with suffix or not by itself, no path.
- *sna*: no suffix function name

- *suf*: suffix only

**Function name strings**

- *sfc*: function string name

**String Code**

- *spg* = not string, but code, to be interpreted as code later

**Graph String**

- *stg*

### Various non-numeric data objects

**Files**

- *fl* = file object

**Figures**

- *pl* = internal plot object

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
