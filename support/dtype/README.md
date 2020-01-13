Fan Naming Conventions:

To help clarify data types for inputs, add prefix to variable names:

- st = string
    + spt = string path
    + svr = variable string
    + sfc = function string name
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
