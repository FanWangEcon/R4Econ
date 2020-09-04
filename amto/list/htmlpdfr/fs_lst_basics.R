## ----global_options, include = FALSE-----------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Define an array to loop over
ar_fl_mean <- c(10, 20, 30)

# store restuls in named list
ls_mt_res = vector(mode = "list", length = length(ar_fl_mean))
ar_st_names <- paste0('mean', ar_fl_mean)
names(ls_mt_res) <- ar_st_names

# Loop and generat a list of dataframes
for (it_fl_mean in seq(1, length(ar_fl_mean))) {
  fl_mean = ar_fl_mean[it_fl_mean]

  # dataframe
  set.seed(it_fl_mean)
  tb_combine <- as_tibble(
    matrix(rnorm(4,mean=fl_mean,sd=1), nrow=2, ncol=3)
    ) %>%
    rowid_to_column(var = "id") %>%
    rename_all(~c(c('id','var1','varb','vartheta')))

  ls_mt_res[[it_fl_mean]] = tb_combine
}

# Retrieve elements
print(ls_mt_res[[1]])
print(ls_mt_res$mean10)
print(ls_mt_res[['mean10']])

# Print via Loop 
for (it_fl_mean in seq(1, length(ar_fl_mean))) {
  tb_combine = ls_mt_res[[it_fl_mean]]
  print(tb_combine)
}


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Define Lists
ls_num <- list(1,2,3)
ls_str <- list('1','2','3')
ls_num_str <- list(1,2,'3')

# Named Lists
ar_st_names <- c('e1','e2','e3')
ls_num_str_named <- ls_num_str
names(ls_num_str_named) <- ar_st_names

# Add Element to Named List
ls_num_str_named$e4 <- 'this is added'


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Initiate List
ls_abc <- vector(mode = "list", length = 0)
# Add Named Elements to List Sequentially
ls_abc$a = 1
ls_abc$b = 2
ls_abc$c = 'abc\'s third element'
# Get all Names Added to List
ar_st_list_names <- names(ls_abc)
# Print list in a loop
print(ls_abc)
for (it_list_ele_ctr in seq(1,length(ar_st_list_names))) {
  st_list_ele_name <- ar_st_list_names[it_list_ele_ctr]
  st_list_ele_val <- ls_abc[it_list_ele_ctr]
  print(paste0(st_list_ele_name,'=',st_list_ele_val))
}


## ----------------------------------------------------------------------------------------------------------------------------------------------
# list to String printing function
ffi_lst2str <- function(ls_list, st_desc, bl_print=TRUE) {

  # string desc
  if(missing(st_desc)){
    st_desc <- deparse(substitute(ls_list))
  }

  # create string
  st_string_from_list = paste0(paste0(st_desc, ':'),
                               paste(names(ls_list), ls_list, sep="=", collapse=";" ))

  if (bl_print){
    print(st_string_from_list)
  }
}

# print full
ffi_lst2str(ls_num)
ffi_lst2str(ls_str)
ffi_lst2str(ls_num_str)
ffi_lst2str(ls_num_str_named)

# print subset
ffi_lst2str(ls_num[2:3])
ffi_lst2str(ls_str[2:3])
ffi_lst2str(ls_num_str[2:4])
ffi_lst2str(ls_num_str_named[c('e2','e3','e4')])


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Dimensions
it_M <- 2
it_Q <- 3
it_N <- it_M*it_Q

# Initiate an Empty MxQ=N element list
ls_2d_flat <- vector(mode = "list", length = it_N)
ls_2d <- ls_2d_flat

# Named flat
ls_2d_flat_named <- ls_2d_flat
names(ls_2d_flat_named) <- paste0('e',seq(1,it_N))
ls_2d_named <- ls_2d_flat_named

# Reshape
dim(ls_2d) <- c(it_M, it_Q)
# named 2d list can not carry 1d name after reshape
dim(ls_2d_named) <- c(it_M, it_Q)


## ----------------------------------------------------------------------------------------------------------------------------------------------
# display
ffi_lst2str(ls_2d_flat_named)
# print(ls_2d_flat_named)
ffi_lst2str(ls_2d_named)
print(ls_2d_named)


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Select Values, double bracket to select from 2dim list
print('ls_2d[[1,2]]')
print(ls_2d[[1,2]])


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Dimensions
it_M <- 3
it_Q <- 4
it_N <- it_M*it_Q

# Initiate an Empty MxQ=N element list
ls_2d_flat_named <- vector(mode = "list", length = it_N)
dim(ls_2d_flat_named) <- c(it_M, it_Q)

# Fill with values
for (it_Q_ctr in seq(1,it_Q)) {
  for (it_M_ctr in seq(1,it_M)) {
    # linear index
    ls_2d_flat_named[[it_M_ctr, it_Q_ctr]] <- (it_Q_ctr-1)*it_M+it_M_ctr
  }
}

# Replace row names, note rownames does not work
dimnames(ls_2d_flat_named)[[1]] <- paste0('row',seq(1,it_M))
dimnames(ls_2d_flat_named)[[2]] <- paste0('col',seq(1,it_Q))

# Element Specific Names
names(ls_2d_flat_named) <- paste0('e',seq(1,it_N))

# Convert to Matrix
tb_2d_flat_named <- as_tibble(ls_2d_flat_named) %>% unnest()
mt_2d_flat_named <- as.matrix(tb_2d_flat_named)


## ----------------------------------------------------------------------------------------------------------------------------------------------
# These are not element names, can still name each element
# display
print('ls_2d_flat_named')
print(ls_2d_flat_named)
print('tb_2d_flat_named')
print(tb_2d_flat_named)
print('mt_2d_flat_named')
print(mt_2d_flat_named)


## ----------------------------------------------------------------------------------------------------------------------------------------------
# Select elements with with dimnames
ffi_lst2str(ls_2d_flat_named[['row2','col2']])
# Select elements with element names
ffi_lst2str(ls_2d_flat_named[['e5']])
# Select elements with index
ffi_lst2str(ls_2d_flat_named[[5]])

