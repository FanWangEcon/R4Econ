# Naming Conventions
mp_valpol_out
## Across Language Naming Conventions

### Numeric Structures

- *fl*: float
- *it*: integer
- *ar*: single dimensional array
- *mt*: two dimensional matrix
- *ts*: three dimensional tensor
- *mn*: any larger than two dimensional array

### Other Structures

- *pl*: plot or figure object
- *ob*: a generic object of any time
- *fc*: a function anonymous function for example, function to be passed as parameters


### Strings

- *st*: any kind of words, single sentence.

#### Special Strings

**Strings for Special Purposes**

- *stf*: prefix for entire string files, all lines, including read in and to be reading in.
- *stm*: time variables values etc that can used for calculations	
- *stg*: graph strings

**Strings but not Actually Strings**

- *svr*: variable string, as in data variable name
- *sfc*: function string name
- *spg*: not string, but code, to be interpreted as code later

#### String Path and File Names

- *spt*: absolute string path, path only, does not include name.
- *spn*: absolute string path, including name.

- *srt*: relative file path, no name
- *srn*: relative file path, with name

- *snm*: function name with suffix, no path.
- *sna*: no suffix function name

- *suf*: suffix only

## R Conventions

## Matlab Conventions

- *ar*: matlab array single
- *mt*: matlab array matrix
- *ts*: matlab array three dimensional
- *cl*: matlab cell
	+ *cln*: cell matrix, meaning some n by m cells
- *mp*: matlab container 

## python conventions

- *co*: class object

## Various non-numeric data objects

**Files**

- *fl* = file object

**Figures**

- *pl* = internal plot object

- oj = some kind of generic anything object
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
