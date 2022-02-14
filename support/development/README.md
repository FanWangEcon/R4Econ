# Guide

The folder contains various Rmd Files help to generate bookdown and index webpages for various projects.

## Working with reticulate and python

When running the python portion of the code, could face various errors, check the following:

1. Open up anaconda prompt, and make sure *wk_pyfan* conda env is installed, see */Tex4Econ/nontex/install/windows/fn_installations.Rmd* for installation related information.
2. Make sure Anaconda is on windows path, so that from within windows standard terminal (not anaconda terminal) can *activate wk_pyfan*
3. In *C:/Users/fan/R4Econ/.Rprofile*, check at the env is set to the *wk_pyfan* conda env, before reticulate is called.
```{r}
Sys.setenv(RETICULATE_PYTHON = "C:/ProgramData/Anaconda3/envs/wk_pyfan/python.exe")
```

## Bookdown Errors and Debugging

### Errors and Paths

When copying project code from one to another, easy to not fully change all project name, so double check tha:

- they are changed in the Rmd files inside the development folder
- *_boodkown.yml* file has the right project names

## Matlab package documentation

### Development and Documentation Sequence

Follow this procedure in developing a matlab package

1. Develop functions
    + Write some script code, get it to work
    + Put the code inside a function
2. Documentation of the function:
    + Write function summary: i) super short summary as title; ii) slightly longer summary as well
    + List out function key parameters (this is not normally done in matlab)
    + List of key invoke examples, and describe differences between invoke examples
    + Check on documentation comment lines with the function.
3. Vignette for functions:
    + Function heading desc can copy function summary desc
    + Key examples with key parameters
    + YML contents
4. Format
    + Go format the m file

Maintain consistency in documentation, MLX file should simply contain documentation from YML and m files:

- Detailed parameter documentation for functions should also be copied over to MLX file
- Bullet-point based yml function description should also appear in MLX file.


### Matlab Error

#### After installing matlab functions are not on path

This might be because the path for function was set incorrectly, to set the path correctly:

1. prj file
2. Toolbox Folder Tab
3. select a root folder for the toolbox
4. Select *PrjOptiSNW/PrjOptiSNW* rather than the root *PrjOptiSNW*.

#### Package can not be installed

Check if there is a *info.xml* file in the *PrjOptiSNW/PrjOptiSNW* folder. Help location line should appear as:

> <help_location>./</help_location>

There should be an automatically generated file demos.xml
