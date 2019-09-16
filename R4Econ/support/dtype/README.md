Fan Naming Conventions:

To help clarify data types for inputs, add prefix to variable names:

- st = string
    + spt = string path
- fl = float
- it = integer
- ar = single dimensional array
  + ar.st
  + ar.fl
- mt = matrix
- ts = higher dimensions matrix, 3d tensor
- ls = list
  + list.ar.st: list of arrays of strings
- sc = structure, generic large store

data set structures

- df = dataframe
- tb = tibble

acronyms

- drv: discrete random variable
- smp: sample

file names

- ff: for fan packages files
    + should first be tested in a folder, should in principle be ff_foldername short
- fs: for fan scripts
    + fst: fan script test, most likely a script that tests a particular ff function, these should be Rmd Files
    + could be small files to test ff files for example
