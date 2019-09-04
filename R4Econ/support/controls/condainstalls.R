#############################
# Fresh Install of Anaconda: Python + R
#############################

1. Delete Conda from Program, Delete R, Delete R-Studio
    + also in run: %appdata%\RStudio, delete the folder
    + also in run: %localappdata%\RStudio-Desktop, delete the folder
2. Download and Install Anaconda
    + https://www.anaconda.com/distribution/, windows, Python 3.7, 64-bit
3. With Anaconda Installed, Open up Anaconda Prompt:
    a. conda update anaconda-navigator
    b. conda update --all
    c. conda install -c r r
    d. conda install -c r r-essentials
4. Search for Path in Windows, update PATH globally, include:
    + C:\ProgramData\Anaconda3\
    + C:\ProgramData\Anaconda3\Scripts\
    + open up regular windows command prompt, type in: r, and then python, see if can enter
    + also check: python --version
5. Download R-studio
    + Upon first opening of R-Studio, will be asked to specify where R is installed:
    + R is installed at: C:\ProgramData\Anaconda3\Lib\R\


#############################
# Fresh Install of Matlab
#############################

1. log into Mathworks
2. UH account downloda latest license
3. Pick packages and install
4. Delete previous version
5. Open up Matlab, type in: matlabroot

#############################
# Other Updates
#############################

1. git update-git-for-windows

#############################
# Update Anaconda Navigator
#############################
conda update anaconda-navigator

#############################
# Conda Packages
#############################

# This is a listing of Packagse to Install from Conda
# Use minimal numbers of packages, install packages from conda-forge
# From Anaconda Prompt right click run as Administrator

#############################
# Jupyter Lab Related
#############################

# Set up to install jupyterlab addon packages
conda upgrade -c conda-forge jupyterlab
conda install -c conda-forge nodejs

# Allow for Directory View in JupyterLab
jupyter labextension install jupyterlab_filetree


#############################
# R Related
#############################

# R kernel
conda install -c r r-irkernel

# Tidyverse
# https://anaconda.org/conda-forge/r-tidyverse
conda install -c r r-tidyverse

# Linear Regression Package
# https://anaconda.org/conda-forge/r-aer
conda install -c conda-forge r-aer

# Nonlinear Regression Package
# https://anaconda.org/conda-forge/r-minpack.lm
conda install -c conda-forge r-minpack.lm

# Allowing for Invoking Matlab Files from Inside R
conda install -c conda-forge r-matlab

#
