# Data Type Checking
lambda <- c(0,2,3)
length(lambda)

lambda <- 1
length(lambda)

if(!is.atomic(lambda)){
  print('is not atomic')
  lambda <- lambda[1]
} else {
  print('is atomic')
}
