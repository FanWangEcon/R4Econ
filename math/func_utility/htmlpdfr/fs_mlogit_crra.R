## ----global_options, include = FALSE------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## -----------------------------------------------------------------------------------------------------------------------------
UA11 = -0.5
UA12 = -1
UA21 = -0.6
UA22 = -1.1
fl_EV_A = log(exp(UA11) + exp(UA12)) + log(exp(UA21) + exp(UA22))
print(fl_EV_A)


## -----------------------------------------------------------------------------------------------------------------------------
UB11 = -0.1
UB12 = -0.65
UB21 = -0.21
UB22 = -0.2
fl_EV_B = log(exp(UB11) + exp(UB12)) + log(exp(UB21) + exp(UB22))
print(fl_EV_B)


## -----------------------------------------------------------------------------------------------------------------------------
ar_increase = seq(0, 0.5, length.out=10)
fl_EV_A = log(exp(UA11*(1+ar_increase)) + exp(UA12*(1+ar_increase))) + log(exp(UA21*(1+ar_increase)) + exp(UA22*(1+ar_increase)))
print(fl_EV_A)


## -----------------------------------------------------------------------------------------------------------------------------
gamma = 1.3
# Define utility Function
ffi_crra <- function(fl_c){
  fl_U = (fl_c^(1-gamma))/(1-gamma)
  return(fl_U)
}


## -----------------------------------------------------------------------------------------------------------------------------
fl_increment = 1.5
CA11 = -0.5+fl_increment
CA12 = -1+fl_increment
CA21 = -0.6+fl_increment
CA22 = -1.1+fl_increment
fl_EV_A = log(exp(ffi_crra(CA11)) + exp(ffi_crra(CA12))) + log(exp(ffi_crra(CA21)) + exp(ffi_crra(CA22)))
print(fl_EV_A)


## -----------------------------------------------------------------------------------------------------------------------------
fl_increment = 3
CB11 = -0.1+fl_increment
CB12 = -0.65+fl_increment
CB21 = -0.21+fl_increment
CB22 = -0.2+fl_increment
fl_EV_B = log(exp(ffi_crra(CB11)) + exp(ffi_crra(CB12))) + log(exp(ffi_crra(CB21)) + exp(ffi_crra(CB22)))
print(fl_EV_B)


## -----------------------------------------------------------------------------------------------------------------------------
ar_increase = seq(0, 3, length.out=10)
# Method 1
fl_EV_A_with_c_increments = 
  log(exp(ffi_crra(CA11*(1+ar_increase))) + 
        exp(ffi_crra(CA12*(1+ar_increase)))) + 
  log(exp(ffi_crra(CA21*(1+ar_increase))) + 
        exp(ffi_crra(CA22*(1+ar_increase))))
print(fl_EV_A_with_c_increments)
# Method 2
fl_EV_A_with_c_increments_m2 = 
  log(exp((1+ar_increase)^(1-gamma)*ffi_crra(CA11)) + 
        exp((1+ar_increase)^(1-gamma)*ffi_crra(CA12))) + 
  log(exp((1+ar_increase)^(1-gamma)*ffi_crra(CA21)) + 
        exp((1+ar_increase)^(1-gamma)*ffi_crra(CA22)))
print(fl_EV_A_with_c_increments_m2)
# Method 3
fl_EV_A_with_c_increments_m3 = 
  log(exp((1+ar_increase)^(1-gamma))*exp(ffi_crra(CA11)) + 
        exp((1+ar_increase)^(1-gamma))*exp(ffi_crra(CA12))) + 
  log(exp((1+ar_increase)^(1-gamma))*exp(ffi_crra(CA21)) + 
        exp((1+ar_increase)^(1-gamma))*exp(ffi_crra(CA22)))
print(fl_EV_A_with_c_increments_m3)
# Method 3
fl_EV_A_with_c_increments_m3 = 
  (1+ar_increase)^(1-gamma) + 
  log(exp(ffi_crra(CA11)) + exp(ffi_crra(CA12))) + 
  (1+ar_increase)^(1-gamma) + 
  log(exp(ffi_crra(CA21)) + exp(ffi_crra(CA22)))
print(fl_EV_A_with_c_increments_m3)

