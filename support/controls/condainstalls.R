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
