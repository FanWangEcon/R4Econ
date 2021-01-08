# Naming Conventions

## Across Language Naming Conventions

### Numeric Structures

- *fl*: float
- *it*: integer
- *ar*: single dimensional array
- *mt*: two dimensional matrix
- *ts*: three dimensional tensor
- *mn*: any larger than two dimensional array

And a special name is

- *gn*: stands for generic type, could be typed differently, the function will parse the parameter type and then behave accordingly.

### Other Structures

- *tp*: immutable tuple if tuple is supported by the language
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

- *tb*: a tibble
- *df*: a dataframe

## Matlab Conventions

- *cl*: matlab cell
	+ *cln*: cell matrix, meaning some n by m cells
- *mp*: matlab container
- *sa*: structure array

## python conventions

- *co*: class object
