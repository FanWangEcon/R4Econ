# Testing some basic function properties
test_func <- function(x, param_opt = NULL, z = 'abc'){
  if(is.null(param_opt)){
    print(paste0('parameter is null:', x, ', pz:', z))
  } else {
    print(paste0('parameter is not null:', x, ', param:', param_opt, ', pz:', z))
  }
}

test_func('a')
test_func('a', z='efg')
test_func('a', param_opt='qqq', z='efg')
