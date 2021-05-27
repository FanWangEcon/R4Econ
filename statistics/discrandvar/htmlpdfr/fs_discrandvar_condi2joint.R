## ----global_options, include = FALSE---------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
ffi_gen_rand_joint_mass <- function(it_seed = 123,
                                    it_Q = 2, it_M = 2,
                                    bl_verbose = FALSE) {

  # set random seed
  set.seed(it_seed)

  # Generate prob list
  ls_2d <- vector(mode = "list", length = it_Q*it_M)
  dim(ls_2d) <- c(it_Q, it_M)

  # Random joint mass
  ar_rand <- runif(it_Q*it_M)
  ar_rand <- ar_rand/sum(ar_rand)

  # Fill with values
  it_ctr <- 0
  for (it_Q_ctr in seq(1,it_Q)) {
    for (it_M_ctr in seq(1,it_M)) {
      # linear index
      ls_2d[[it_M_ctr, it_Q_ctr]] <- ar_rand[(it_Q_ctr-1)*it_M+it_M_ctr]
    }
  }

  # Replace row names, note rownames does not work
  dimnames(ls_2d)[[1]] <- paste0('E',seq(1,it_M))
  dimnames(ls_2d)[[2]] <- paste0('A',seq(1,it_Q))

  # rename
  ls_prob_joint_E_A <- ls_2d
  mt_prob_joint_E_A <- matrix(unlist(ls_prob_joint_E_A), ncol=it_M, byrow=F)

  if (bl_verbose) {
    print('ls_prob_joint_E_A')
    print(ls_prob_joint_E_A)
    print(mt_prob_joint_E_A)
  }

  # return
  return(list(mt_prob_joint_E_A=mt_prob_joint_E_A,
              ls_prob_joint_E_A=ls_prob_joint_E_A))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
ffi_gen_condi_unemploy_prob_2by2 <- function(mt_prob_joint_E_A,
                                        fl_alpha = 0.25, fl_beta = 0.05,
                                        fl_gamma = 0.31, fl_delta = 0.50,
                                        bl_verbose = FALSE) {

  # From joint probability, generate conditional probabilities
  fl_F <- mt_prob_joint_E_A[1,1]/sum(mt_prob_joint_E_A[1,])
  fl_B <- mt_prob_joint_E_A[2,1]/sum(mt_prob_joint_E_A[2,])
  fl_C <- mt_prob_joint_E_A[1,1]/sum(mt_prob_joint_E_A[,1])
  fl_D <- mt_prob_joint_E_A[1,2]/sum(mt_prob_joint_E_A[,2])
  if (bl_verbose) {
    print(paste0('fl_F=', fl_F, ',fl_B=',fl_B,',fl_C=',fl_C,',fl_D=',fl_D))
  }

  # Also generate random W X Y Z
  # ar_b <- runif(4)

  # fl_Delta_A_true <- 0.05
  # fl_Delta_E_true <- 0.11

  # fl_alpha_true <- 0.25
  # fl_beta_true <- fl_alpha_true + fl_Delta_A_true
  # fl_gamma_true <- fl_alpha_true + fl_Delta_E_true
  # fl_delta_true <- fl_alpha_true + fl_Delta_E_true + fl_Delta_A_true

  # fl_beta_true <- 0.31
  # fl_gamma_true <- 0.50
  # fl_delta_true <- 0.16

  fl_W <- fl_alpha*fl_F + fl_beta*(1-fl_F)
  fl_X <- fl_gamma*fl_B + fl_delta*(1-fl_B)
  fl_Y <- fl_alpha*fl_C + fl_gamma*(1-fl_C)
  fl_Z <- fl_beta*fl_D + fl_delta*(1-fl_D)
  fl_V <- mt_prob_joint_E_A[1,1]*fl_alpha_true +
    mt_prob_joint_E_A[1,2]*fl_beta_true +
    mt_prob_joint_E_A[2,1]*fl_gamma_true +
    mt_prob_joint_E_A[2,2]*fl_delta_true
  ar_b <- c(fl_W, fl_X, fl_Y, fl_Z)

  if (bl_verbose) {
    print(paste0('ar_b=',ar_b))
    print(paste0('fl_V=',fl_V))
  }

  # return
  return(list(F=fl_F, B=fl_B, C=fl_C, D=fl_D,
    W=fl_W, X=fl_X, Y=fl_Y, Z=fl_Z, V=fl_V))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# JOint mass of population at all cells
ls_ffi_rand_joint_mass <- ffi_gen_rand_joint_mass(it_seed = 123, it_Q = 2, it_M = 2, bl_verbose = TRUE)
mt_prob_joint_E_A <- ls_ffi_rand_joint_mass$mt_prob_joint_E_A
# retlinear restrictions for conditional unemployment probabilities
fl_Delta_A_true <- 0.05
fl_Delta_E_true <- 0.11
fl_alpha_true <- 0.25
fl_beta_true <- fl_alpha_true + fl_Delta_A_true
fl_gamma_true <- fl_alpha_true + fl_Delta_E_true
fl_delta_true <- fl_alpha_true + fl_Delta_E_true + fl_Delta_A_true
ls_FBCDWXYZV <- ffi_gen_condi_unemploy_prob_2by2(mt_prob_joint_E_A,
  fl_alpha = fl_alpha_true, fl_beta = fl_beta_true,
  fl_gamma = fl_gamma_true, fl_delta = fl_delta_true, bl_verbose=TRUE)
fl_F <- ls_FBCDWXYZV$F
fl_B <- ls_FBCDWXYZV$B
fl_C <- ls_FBCDWXYZV$C
fl_D <- ls_FBCDWXYZV$D
fl_W <- ls_FBCDWXYZV$W
fl_X <- ls_FBCDWXYZV$X
fl_Y <- ls_FBCDWXYZV$Y
fl_Z <- ls_FBCDWXYZV$Z
fl_V <- ls_FBCDWXYZV$V


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# does not matter if joint sum is used or the alternative
bl_use_joint_sum <- FALSE
if (bl_use_joint_sum) {
  # Construct The coefficent Matrix
  mt_OMEGA = t(
    matrix(data=c(fl_F, 1-fl_F, 0, 0,
                  0, 0, fl_B, 1-fl_B,
                  fl_C, 0, 1-fl_C, 0,
                  mt_prob_joint_E_A[1,1],  mt_prob_joint_E_A[1,2],
                  mt_prob_joint_E_A[2,1],  mt_prob_joint_E_A[2,2]), nrow=4, ncol=4))
} else {
  # Construct The coefficent Matrix
  mt_OMEGA = t(matrix(data=c(fl_F, 1-fl_F, 0, 0,
                             0, 0, fl_B, 1-fl_B,
                             fl_C, 0, 1-fl_C, 0,
                             0, fl_D, 0, 1-fl_D), nrow=4, ncol=4))

}
# rank Check with the qr function:
print(qr(mt_OMEGA))
# rank Check with the qr function:


## --------------------------------------------------------------------------------------------------------------------------------------------------------
ffi_solve_2by2_rectilinear <- function(fl_F, fl_B, fl_C,
                                       fl_W, fl_X, fl_Y,
                                       bl_verbose=FALSE) {

  # Construct The coefficent Matrix
  mt_OMEGA_hat = t(matrix(
    data=c(1, 1-fl_F, 0,
           1, 1-fl_B, 1,
           1, 0, 1-fl_C), nrow=3, ncol=3))

  # rank Check with the qr function:
  if (bl_verbose) {
    print(qr(mt_OMEGA_hat))
  }

  # bhat
  ar_b_hat <- c(fl_W, fl_X, fl_Y)

  # solve
  ar_solution <- solve(mt_OMEGA_hat, ar_b_hat)

  # Get alpha and Delta from solution
  fl_alpha <- ar_solution[1]
  fl_Delta_A <- ar_solution[2]
  fl_Delta_E <- ar_solution[3]
  if (bl_verbose) {
    print(paste0('fl_Delta_A=',fl_Delta_A))
    print(paste0('fl_Delta_E=',fl_Delta_E))
  }

  # Get beta gamma , delta
  fl_beta <- fl_alpha + fl_Delta_A
  fl_gamma <- fl_alpha + fl_Delta_E
  fl_delta <- fl_gamma + fl_Delta_A
  if (bl_verbose) {
    print(paste0('fl_alpha=',fl_alpha))
    print(paste0('fl_beta=',fl_beta))
    print(paste0('fl_gamma=',fl_gamma))
    print(paste0('fl_delta=',fl_delta))
    if (abs(ar_b[1] - (fl_alpha*fl_F + fl_beta*(1-fl_F))) < 1e-10) {
      print('W matched')
    }
    if (abs(ar_b[2] - (fl_gamma*fl_B + fl_delta*(1-fl_B))) < 1e-10) {
      print('X matched')
    }
    if (abs(ar_b[3] - (fl_alpha*fl_C + fl_gamma*(1-fl_C))) < 1e-10) {
      print('Y matched')
    }
    if (abs(ar_b[4] - (fl_beta*fl_D + fl_delta*(1-fl_D))) < 1e-10) {
      print('Z matched')
    }
    if (abs(fl_V - (mt_prob_joint_E_A[1,1]*fl_alpha +
                    mt_prob_joint_E_A[1,2]*fl_beta +
                    mt_prob_joint_E_A[2,1]*fl_gamma +
                    mt_prob_joint_E_A[2,2]*fl_delta)) < 1e-10) {
      print('V matched')
    }
  }

  return(list(Delta_A=fl_Delta_A, Delta_E=fl_Delta_E,
              alpha=fl_alpha, beta=fl_beta,
              gamma=fl_gamma, delta=fl_delta))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
for (it_i in c(1,2,3)) {

  # JOint mass of population at all cells
  ls_ffi_rand_joint_mass <- ffi_gen_rand_joint_mass(
    it_seed = 123, it_Q = 2, it_M = 2, bl_verbose = FALSE)
  mt_prob_joint_E_A <- ls_ffi_rand_joint_mass$mt_prob_joint_E_A

  # retlinear restrictions for conditional unemployment probabilities
  fl_alpha_true <- 0.25
  if (it_i == 1) {
    fl_Delta_A_true <- 0.05
    fl_Delta_E_true <- 0.11
  } else if (it_i == 2) {
    fl_Delta_A_true <- 0.21
    fl_Delta_E_true <- 0.03
  } else if (it_i == 3) {
    fl_Delta_A_true <- -0.05
    fl_Delta_E_true <- -0.11
  }

  fl_beta_true <- fl_alpha_true + fl_Delta_A_true
  fl_gamma_true <- fl_alpha_true + fl_Delta_E_true
  fl_delta_true <- fl_alpha_true + fl_Delta_E_true + fl_Delta_A_true
  ls_FBCDWXYZV <- ffi_gen_condi_unemploy_prob_2by2(
    mt_prob_joint_E_A,
    fl_alpha = fl_alpha_true, fl_beta = fl_beta_true,
    fl_gamma = fl_gamma_true, fl_delta = fl_delta_true, bl_verbose=FALSE)

  # call solution function
  fl_F <- ls_FBCDWXYZV$F
  fl_B <- ls_FBCDWXYZV$B
  fl_C <- ls_FBCDWXYZV$C
  fl_D <- ls_FBCDWXYZV$D
  fl_W <- ls_FBCDWXYZV$W
  fl_X <- ls_FBCDWXYZV$X
  fl_Y <- ls_FBCDWXYZV$Y
  fl_Z <- ls_FBCDWXYZV$Z
  fl_V <- ls_FBCDWXYZV$V
  ls_solution <- ffi_solve_2by2_rectilinear(
    fl_F, fl_B, fl_C,
    fl_W, fl_X, fl_Y)

  fl_alpha <- ls_solution$alpha
  fl_beta <- ls_solution$beta
  fl_gamma <- ls_solution$gamma
  fl_delta <- ls_solution$delta

  # check
  print(paste0('it_i=', it_i))
  print(paste0('fl_alpha=', fl_alpha, ', fl_alpha_true=', fl_alpha_true))
  print(paste0('fl_beta=', fl_beta, ', fl_beta_true=', fl_beta_true))
  print(paste0('fl_gamma=', fl_gamma, ', fl_gamma_true=', fl_gamma_true))
  print(paste0('fl_delta=', fl_delta, ', fl_delta_true=', fl_delta_true))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
for (it_i in c(1,2,3)) {

  # JOint mass of population at all cells
  ls_ffi_rand_joint_mass <- ffi_gen_rand_joint_mass(
    it_seed = 123, it_Q = 2, it_M = 2, bl_verbose = FALSE)
  mt_prob_joint_E_A <- ls_ffi_rand_joint_mass$mt_prob_joint_E_A

  # retlinear restrictions for conditional unemployment probabilities
  if (it_i == 1) {
    fl_alpha_true <- 0.25
    fl_Delta_A_true <- 0.05
    fl_Delta_E_true <- 0.11
    fl_Delta_AE_true <- 0.10
  } else if (it_i == 2) {
    fl_alpha_true <- 0.55
    fl_Delta_A_true <- 0.21
    fl_Delta_E_true <- 0.03
    fl_Delta_AE_true <- 0.15
  } else if (it_i == 3) {
    fl_alpha_true <- 0.15
    fl_Delta_A_true <- -0.05
    fl_Delta_E_true <- -0.11
    fl_Delta_AE_true <- 0.30
  }

  fl_beta_true <- fl_alpha_true + fl_Delta_A_true
  fl_gamma_true <- fl_alpha_true + fl_Delta_E_true
  fl_delta_true <- fl_alpha_true + fl_Delta_AE_true
  ls_FBCDWXYZV <- ffi_gen_condi_unemploy_prob_2by2(
    mt_prob_joint_E_A,
    fl_alpha = fl_alpha_true, fl_beta = fl_beta_true,
    fl_gamma = fl_gamma_true, fl_delta = fl_delta_true, bl_verbose=FALSE)

  # call solution function
  fl_F <- ls_FBCDWXYZV$F
  fl_B <- ls_FBCDWXYZV$B
  fl_C <- ls_FBCDWXYZV$C
  fl_D <- ls_FBCDWXYZV$D
  fl_W <- ls_FBCDWXYZV$W
  fl_X <- ls_FBCDWXYZV$X
  fl_Y <- ls_FBCDWXYZV$Y
  fl_Z <- ls_FBCDWXYZV$Z
  fl_V <- ls_FBCDWXYZV$V
  ls_solution <- ffi_solve_2by2_rectilinear(
    fl_F, fl_B, fl_C,
    fl_W, fl_X, fl_Y)

  fl_alpha <- ls_solution$alpha
  fl_beta <- ls_solution$beta
  fl_gamma <- ls_solution$gamma
  fl_delta <- ls_solution$delta

  # check
  print(paste0('it_i=', it_i))
  print(paste0('fl_alpha=', fl_alpha, ', fl_alpha_true=', fl_alpha_true))
  print(paste0('fl_beta=', fl_beta, ', fl_beta_true=', fl_beta_true))
  print(paste0('fl_gamma=', fl_gamma, ', fl_gamma_true=', fl_gamma_true))
  print(paste0('fl_delta=', fl_delta, ', fl_delta_true=', fl_delta_true))
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
ffi_gen_condi_unemploy_prob_3by3 <- function(mt_prob_joint_E_A, mt_alpha, bl_verbose = FALSE) {

  # From joint probability, generate conditional probabilities
  fl_B21 <- mt_prob_joint_E_A[1,2]/sum(mt_prob_joint_E_A[1,])
  fl_B31 <- mt_prob_joint_E_A[1,3]/sum(mt_prob_joint_E_A[1,])

  fl_B22 <- mt_prob_joint_E_A[2,2]/sum(mt_prob_joint_E_A[2,])
  fl_B32 <- mt_prob_joint_E_A[2,3]/sum(mt_prob_joint_E_A[2,])

  fl_B23 <- mt_prob_joint_E_A[3,2]/sum(mt_prob_joint_E_A[3,])
  fl_B33 <- mt_prob_joint_E_A[3,3]/sum(mt_prob_joint_E_A[3,])

  fl_C21 <- mt_prob_joint_E_A[2,1]/sum(mt_prob_joint_E_A[,1])
  fl_C31 <- mt_prob_joint_E_A[3,1]/sum(mt_prob_joint_E_A[,1])

  fl_C22 <- mt_prob_joint_E_A[2,2]/sum(mt_prob_joint_E_A[,2])
  fl_C32 <- mt_prob_joint_E_A[3,2]/sum(mt_prob_joint_E_A[,2])

  if (bl_verbose) {
    print(paste0('fl_B21=', fl_B21, ',fl_B31=',fl_B31))
    print(paste0('fl_B22=', fl_B22, ',fl_B22=',fl_B22))
    print(paste0('fl_B23=', fl_B23, ',fl_B33=',fl_B33))
    print(paste0('fl_C21=', fl_C21, ',fl_C31=',fl_C31))
    print(paste0('fl_C22=', fl_C22, ',fl_C32=',fl_C32))
  }

  # Generate random W X Y
  fl_V1 <- mt_alpha[1,1]*(1 - fl_B21 - fl_B31) + mt_alpha[2,1]*fl_B21 + mt_alpha[3,1]*fl_B31
  fl_V2 <- mt_alpha[1,2]*(1 - fl_B22 - fl_B32) + mt_alpha[2,2]*fl_B22 + mt_alpha[3,2]*fl_B32
  fl_V3 <- mt_alpha[1,3]*(1 - fl_B23 - fl_B33) + mt_alpha[2,3]*fl_B23 + mt_alpha[3,3]*fl_B33
  fl_W1 <- mt_alpha[1,1]*(1 - fl_C21 - fl_C31) + mt_alpha[1,2]*fl_C21 + mt_alpha[1,3]*fl_C31
  fl_W2 <- mt_alpha[2,1]*(1 - fl_C22 - fl_C32) + mt_alpha[2,2]*fl_C22 + mt_alpha[2,3]*fl_C32
  ar_b <- c(fl_V1, fl_V2, fl_V3, fl_W1, fl_W2)

  if (bl_verbose) {
    print(paste0('ar_b=',ar_b))
  }

  # return
  return(ar_b)
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
ffi_solve_3by3_rectilinear <- function(mt_prob_joint_E_A, ar_b, bl_verbose=FALSE) {

  # From joint probability, generate conditional probabilities
  fl_B21 <- mt_prob_joint_E_A[1,2]/sum(mt_prob_joint_E_A[1,])
  fl_B31 <- mt_prob_joint_E_A[1,3]/sum(mt_prob_joint_E_A[1,])

  fl_B22 <- mt_prob_joint_E_A[2,2]/sum(mt_prob_joint_E_A[2,])
  fl_B32 <- mt_prob_joint_E_A[2,3]/sum(mt_prob_joint_E_A[2,])

  fl_B23 <- mt_prob_joint_E_A[3,2]/sum(mt_prob_joint_E_A[3,])
  fl_B33 <- mt_prob_joint_E_A[3,3]/sum(mt_prob_joint_E_A[3,])

  fl_C21 <- mt_prob_joint_E_A[2,1]/sum(mt_prob_joint_E_A[,1])
  fl_C31 <- mt_prob_joint_E_A[3,1]/sum(mt_prob_joint_E_A[,1])

  fl_C22 <- mt_prob_joint_E_A[2,2]/sum(mt_prob_joint_E_A[,2])
  fl_C32 <- mt_prob_joint_E_A[3,2]/sum(mt_prob_joint_E_A[,2])

  # Construct The coefficent Matrix
  mt_OMEGA = t(matrix(
    data=c(1, fl_B21+fl_B31, fl_B31, 0, 0,
           1, fl_B22+fl_B32, fl_B32, 1, 0,
           1, fl_B23+fl_B33, fl_B33, 1, 1,
           1, 0, 0, fl_C21+fl_C31, fl_C31,
           1, 1, 0, fl_C22+fl_C32, fl_C32), nrow=5, ncol=5))

  # rank Check with the qr function:
  if (bl_verbose) {
    print(qr(mt_OMEGA_hat))
  }

  # solve
  ar_solution <- solve(mt_OMEGA, ar_b)

  # Get alpha and Delta from solution
  fl_alpha <- ar_solution[1]
  fl_Delta_21 <- ar_solution[2]
  fl_Delta_31 <- ar_solution[3]
  fl_Delta_12 <- ar_solution[4]
  fl_Delta_13 <- ar_solution[5]
  if (bl_verbose) {
    print(paste0('fl_Delta_21=',fl_Delta_21))
    print(paste0('fl_Delta_31=',fl_Delta_31))
    print(paste0('fl_Delta_12=',fl_Delta_12))
    print(paste0('fl_Delta_13=',fl_Delta_13))
  }

  # Get beta gamma , delta
  mt_alpha = matrix(0, 3, 3)
  for (it_row in c(1,2,3)) {
    for (it_col in c(1,2,3)) {

      fl_current_val <- fl_alpha

      if (it_row == 2) {
        fl_current_val <- fl_current_val + fl_Delta_21
      }
      if (it_row == 3) {
        fl_current_val <- fl_current_val + fl_Delta_21 + fl_Delta_31
      }
      if (it_col == 2) {
        fl_current_val <- fl_current_val + fl_Delta_12
      }
      if (it_col == 3) {
        fl_current_val <- fl_current_val + fl_Delta_12 + fl_Delta_13
      }

      mt_alpha[it_row, it_col] <- fl_current_val
    }
  }


  return(mt_alpha)
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
for (it_i in c(1,2,3)) {

  # JOint mass of population at all cells
  ls_ffi_rand_joint_mass <- ffi_gen_rand_joint_mass(
    it_seed = 123, it_Q = 3, it_M = 3, bl_verbose = FALSE)
  mt_prob_joint_E_A <- ls_ffi_rand_joint_mass$mt_prob_joint_E_A

  # retlinear restrictions for conditional unemployment probabilities
  if (it_i == 1) {
    fl_alpha <- 0.1
    fl_Delta_21 <- 0.05
    fl_Delta_31 <- 0.07
    fl_Delta_12 <- 0.09
    fl_Delta_13 <- 0.03
  } else if (it_i == 2) {
    fl_alpha <- 0.20
    fl_Delta_21 <- -0.05
    fl_Delta_31 <- +0.07
    fl_Delta_12 <- 0.09
    fl_Delta_13 <- -0.03
  } else if (it_i == 3) {
    fl_alpha <- 0.15
    fl_Delta_21 <- 0.11
    fl_Delta_31 <- -0.01
    fl_Delta_12 <- 0
    fl_Delta_13 <- +0.1
  }

  mt_alpha_true = matrix(0, 3, 3)
  for (it_row in c(1,2,3)) {
    for (it_col in c(1,2,3)) {

      fl_current_val <- fl_alpha

      if (it_row == 2) {
        fl_current_val <- fl_current_val + fl_Delta_21
      }
      if (it_row == 3) {
        fl_current_val <- fl_current_val + fl_Delta_21 + fl_Delta_31
      }
      if (it_col == 2) {
        fl_current_val <- fl_current_val + fl_Delta_12
      }
      if (it_col == 3) {
        fl_current_val <- fl_current_val + fl_Delta_12 + fl_Delta_13
      }

      mt_alpha_true[it_row, it_col] <- fl_current_val
    }
  }

  ar_b <- ffi_gen_condi_unemploy_prob_3by3(mt_prob_joint_E_A, mt_alpha_true, bl_verbose=FALSE)

  # call solution function
  mt_alpha_solu <- ffi_solve_3by3_rectilinear(mt_prob_joint_E_A, ar_b)

  # check
  print(paste0('it_i=', it_i))
  print('mt_alpha_true')
  print(mt_alpha_true)
  print('mt_alpha_solu')
  print(mt_alpha_solu)

}


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# set seed
set.seed(123)
# loop over alternatives
for (it_i in c(1,2,3)) {

  # JOint mass of population at all cells
  ls_ffi_rand_joint_mass <- ffi_gen_rand_joint_mass(
    it_seed = 123, it_Q = 3, it_M = 3, bl_verbose = FALSE)
  mt_prob_joint_E_A <- ls_ffi_rand_joint_mass$mt_prob_joint_E_A

  # retlinear restrictions for conditional unemployment probabilities
  if (it_i == 1) {
    fl_alpha <- 0.1
    fl_Delta_21 <- 0.05
    fl_Delta_31 <- 0.07
    fl_Delta_12 <- 0.09
    fl_Delta_13 <- 0.03
  } else if (it_i == 2) {
    fl_alpha <- 0.20
    fl_Delta_21 <- -0.05
    fl_Delta_31 <- +0.07
    fl_Delta_12 <- 0.09
    fl_Delta_13 <- -0.03
  } else if (it_i == 3) {
    fl_alpha <- 0.15
    fl_Delta_21 <- 0.11
    fl_Delta_31 <- -0.01
    fl_Delta_12 <- 0
    fl_Delta_13 <- +0.1
  }

  mt_alpha_true = matrix(0, 3, 3)
  for (it_row in c(1,2,3)) {
    for (it_col in c(1,2,3)) {

      fl_current_val <- fl_alpha

      if (it_row == 2) {
        fl_current_val <- fl_current_val + fl_Delta_21 + runif(1)*0.2
      }
      if (it_row == 3) {
        fl_current_val <- fl_current_val + fl_Delta_21 + fl_Delta_31 + runif(1)*0.2
      }
      if (it_col == 2) {
        fl_current_val <- fl_current_val + fl_Delta_12 + runif(1)*0.2
      }
      if (it_col == 3) {
        fl_current_val <- fl_current_val + fl_Delta_12 + fl_Delta_13 + runif(1)*0.2
      }

      mt_alpha_true[it_row, it_col] <- fl_current_val
    }
  }

  ar_b <- ffi_gen_condi_unemploy_prob_3by3(mt_prob_joint_E_A, mt_alpha_true, bl_verbose=FALSE)

  # call solution function
  mt_alpha_solu <- ffi_solve_3by3_rectilinear(mt_prob_joint_E_A, ar_b)

  # check
  print(paste0('it_i=', it_i))
  print('mt_alpha_true')
  print(mt_alpha_true)
  print('mt_alpha_solu')
  print(mt_alpha_solu)

}

