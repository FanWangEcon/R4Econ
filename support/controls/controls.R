# Control Figure Sizes in Jupyter
options(repr.plot.width = 4, repr.plot.height = 4)

# Suppress warning
options(warn=-1) # Suppress Warning Messages
options(warn=0) # do not suppress warning

# Suppress Messages
suppressMessages(some-commands-here) # wrap function call etc in this to avoid messages
# Multi-line message suppression
suppressMessages({
    df_unique_fixed <- read_csv('data_indi_unique.csv');
    df_panel <- read_csv('data_m0t24_g89c1213.csv')
})

# Table Display Rows and Columns
options(repr.matrix.max.rows=50, repr.matrix.max.cols=20)

# Timer
system.time({Some_Function()})
