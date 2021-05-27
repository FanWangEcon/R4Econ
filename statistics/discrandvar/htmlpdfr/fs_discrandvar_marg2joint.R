## ----global_options, include = FALSE---------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Construct The coefficent Matrix
mt_a = t(matrix(data=c(1, 1, 0, 0,
                       0, 0, 1, 1,
                       1, 0, 1, 0,
                       0, 1, 0, 1), nrow=4, ncol=4))
# rank Check with the qr function:
print(qr(mt_a))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Labeling
st_title <- paste0('2 by 2 Joint Mass from Marginal Rectilinear Assumption\n',
                   'Intersecting Area Positive Mass at all Joint Discrete Points\n',
                   'x-axis and y-axis values will never exceed -0.5 or 0.5')
st_subtitle <- paste0('https://fanwangecon.github.io/',
                      'R4Econ/math/discrandvar/htmlpdfr/fs_drm_mass.html')
st_x_label <- 'delta A'
st_y_label <- 'delta E'

# Line 1
x1 <- seq(0, 0.5, length.out=50)
y1 <- 0.5-x1
st_legend1 <- 'Below This Line\n P_A1_E1>0 Restriction'
# Line 2
x2 <- seq(-0.5, 0, length.out=50)
y2 <- 0.5+x2
st_legend2 <- 'Below This Line\n P_A2_E1>0 Restriction'
# Line 3
x3 <- seq(0, 0.5, length.out=50)
y3 <- -0.5+x3
st_legend3 <- 'Above This Line\n P_A1_E2>0 Restriction'
# Line 4
x4 <- seq(-0.5, 0, length.out=50)
y4 <- -0.5-x4
st_legend4 <- 'Above This Line\n P_A1_E2>0 Restriction'

# line lty
st_line_1_lty <- 'solid'
st_line_2_lty <- 'dashed'
st_line_3_lty <- 'dotted'
st_line_4_lty <- 'dotdash'

# Share xlim and ylim
ar_xlim = c(-0.75, 0.75)
ar_ylim = c(-0.75, 0.75)

# Graph
par(new=FALSE, mar=c(5, 4, 4, 10))
plot(x1, y1, type="l", col = 'black', lwd = 2.5, lty = st_line_1_lty,
      xlim = ar_xlim, ylim = ar_ylim,
      ylab = '', xlab = '', yaxt='n', xaxt='n', ann=FALSE)
par(new=T)
plot(x2, y2, type="l", col = 'black', lwd = 2.5, lty = st_line_2_lty,
      xlim = ar_xlim, ylim = ar_ylim,
      ylab = '', xlab = '', yaxt='n', xaxt='n', ann=FALSE)
par(new=T)
plot(x3, y3, type="l", col = 'black', lwd = 2.5, lty = st_line_3_lty,
      xlim = ar_xlim, ylim = ar_ylim,
      ylab = '', xlab = '', yaxt='n', xaxt='n', ann=FALSE)
par(new=T)
plot(x4, y4, type="l", col = 'black', lwd = 2.5, lty = st_line_4_lty,
      xlim = ar_xlim, ylim = ar_ylim,
      ylab = '', xlab = '', yaxt='n', xaxt='n', ann=FALSE)

# CEX sizing Contorl Titling and Legend Sizes
fl_ces_fig_reg = 1
fl_ces_fig_small = 0.75

# R Legend
title(main = st_title, sub = st_subtitle, xlab = st_x_label, ylab = st_y_label,
      cex.lab=fl_ces_fig_reg,
      cex.main=fl_ces_fig_reg,
      cex.sub=fl_ces_fig_small)
axis(1, cex.axis=fl_ces_fig_reg)
axis(2, cex.axis=fl_ces_fig_reg)
grid()

# Legend sizing CEX
legend("topright",
       inset=c(-0.4,0),
       xpd=TRUE,
       c(st_legend1, st_legend2, st_legend3, st_legend4),
       cex = fl_ces_fig_small,
       lty = c(st_line_1_lty, st_line_2_lty, st_line_3_lty, st_line_4_lty),
       title = 'Legends',
       y.intersp=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------
it_warning = 0
it_neg = 0
for (it_rand_seed in 1:1000) {
  # Generate two marginal MASS
  set.seed(it_rand_seed)
  ar_E_marginal <- runif(2)
  # ar_E_marginal <- c(0.01, 0.99)
  # ar_E_marginal <- c(0.49, 0.51)
  ar_E_marginal <- ar_E_marginal/sum(ar_E_marginal)
  ar_A_marginal <- runif(2)
  # ar_A_marginal <- c(0.01, 0.99)
  # ar_A_marginal <- c(0.01, 0.99)
  ar_A_marginal <- ar_A_marginal/sum(ar_A_marginal)
  # print(ar_E_marginal)
  # print(ar_A_marginal)
  # Differences in Marginal Points
  ar_delta_E = diff(ar_E_marginal)/2
  ar_delta_A = diff(ar_A_marginal)/2
  # print(paste0('deltaE + deltaA:', diff(ar_E_marginal) + diff(ar_A_marginal)))
  # some cell negativity condition:
  if (sum(abs(diff(ar_E_marginal))) + sum(abs(diff(ar_A_marginal))) > 1){
    it_warning = it_warning + 1
    # warning('Outside of Diamond, Rectilinear Restriction Leads to Negative Values in Some Cells\n')
  }
  # What is P(A1,E1), implemetning the formula above
  fl_P_A1_E1 = (1 - c(1,1) %*% rbind(ar_delta_E, ar_delta_A) %*% t(t(c(2))))/(4)
  # Getting the Entire P_A_E matrix
  mt_P_A_E = matrix(data=NA, nrow=2, ncol=2)
  for (it_row in 1:length(ar_E_marginal)){
    for (it_col in 1:length(ar_A_marginal)){
      fl_p_value = fl_P_A1_E1
      if (it_row >= 2){
        fl_p_value = fl_p_value + sum(ar_delta_E[1:(it_row-1)])
      }
      if (it_col >= 2){
        fl_p_value = fl_p_value + sum(ar_delta_A[1:(it_col-1)])
      }
      mt_P_A_E[it_row, it_col] = fl_p_value
    }
  }
  # print(mt_P_A_E)
  sum(mt_P_A_E)
  rowSums(mt_P_A_E)
  colSums(mt_P_A_E)
  if (length(mt_P_A_E[mt_P_A_E<0])>0){
      it_neg = it_neg + 1
  }
}
print(paste0('it_warning:',it_warning))
print(paste0('it_neg:',it_neg))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Generate two marginal MASS
it_warning = 0
it_neg = 0
it_concur = 0
for (it_rand_seed in 1:1000) {
  set.seed(it_rand_seed)
  # set.seed(333)
  ar_E_marginal <- runif(3)
  ar_E_marginal <- ar_E_marginal/sum(ar_E_marginal)
  ar_A_marginal <- runif(3)
  ar_A_marginal <- ar_A_marginal/sum(ar_A_marginal)
  # print(ar_E_marginal)
  # print(ar_A_marginal)
  # Differences in Marginal Points
  ar_delta_E_m = diff(ar_E_marginal)
  ar_delta_A_m = diff(ar_A_marginal)
  ar_delta_E = diff(ar_E_marginal)/3
  ar_delta_A = diff(ar_A_marginal)/3
  # some cell negativity condition: this condition is incorrect
  bl_count_warn = FALSE
  for (it_row in 1:length(ar_delta_E)){
    for (it_col in 1:length(ar_delta_A)){
      if ((abs(sum(ar_delta_E_m[1:it_row])) + abs(sum(ar_delta_A_m[1:it_col]))) > 2/4) {
        bl_count_warn = TRUE
      }
      if ((abs(ar_delta_E_m[it_row]) + abs(ar_delta_A_m[it_col])) > 2/4) {
        bl_count_warn = TRUE
      }
    }
  }
  if (bl_count_warn) {
    # if (max(abs(diff(ar_E_marginal))) + max(abs(diff(ar_A_marginal))) > 2/3){
    it_warning = it_warning + 1
    # }
    # warning('Outside of Diamond, Rectilinear Restriction Leads to Negative Values in Some Cells\n')
  }
  # What is P(A1,E1), implemetning the formula above
  fl_P_A1_E1 = (1 - c(1,1) %*% rbind(ar_delta_E, ar_delta_A) %*% t(t(c(3*2, 3))))/(3*3)
  # Getting the Entire P_A_E matrix
  mt_P_A_E = matrix(data=NA, nrow=3, ncol=3)
  for (it_row in 1:length(ar_E_marginal)){
    for (it_col in 1:length(ar_A_marginal)){
      fl_p_value = fl_P_A1_E1
      if (it_row >= 2){
        fl_p_value = fl_p_value + sum(ar_delta_E[1:(it_row-1)])
      }
      if (it_col >= 2){
        fl_p_value = fl_p_value + sum(ar_delta_A[1:(it_col-1)])
      }
      mt_P_A_E[it_row, it_col] = fl_p_value
    }
  }
  # print(mt_P_A_E)
  sum(mt_P_A_E)
  rowSums(mt_P_A_E)
  colSums(mt_P_A_E)
  if (length(mt_P_A_E[mt_P_A_E<0])>0){
      it_neg = it_neg + 1
      if (bl_count_warn) {
        it_concur = it_concur + 1
      }
  }
}
print(paste0('it_warning:',it_warning))
print(paste0('it_neg:',it_neg))
print(paste0('it_concur:',it_concur))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# pi_j=[0.22;0.175;0.16;0.165;0.22]; % Probability of unemployment in 2020 by age groups from Cajner et al. (2020, NBER)
# pi_w=[0.360;0.22;0.17;0.14;0.09]; % Probability of unemployment in 2020 by wage quintiles from Cajner et al. (2020, NBER)
# Generate two marginal MASS
set.seed(111)
# set.seed(333)
ar_E_marginal <- c(0.22, 0.175, 0.16, 0.165, 0.22)
ar_E_marginal <- ar_E_marginal/sum(ar_E_marginal)
ar_A_marginal <- c(0.360, 0.22, 0.17, 0.14, 0.09)
ar_A_marginal <- ar_A_marginal/sum(ar_A_marginal)
print(ar_E_marginal)
print(ar_A_marginal)
# Differences in Marginal Points
ar_delta_E = diff(ar_E_marginal)/5
ar_delta_A = diff(ar_A_marginal)/5
# What is P(A1,E1), implemetning the formula above
fl_P_A1_E1 = (1 - c(1,1) %*% rbind(ar_delta_E, ar_delta_A) %*% t(t(c(20,15,10,5))))/(5*5)
# Getting the Entire P_A_E matrix
mt_P_A_E = matrix(data=NA, nrow=5, ncol=5)
for (it_row in 1:length(ar_E_marginal)){
  for (it_col in 1:length(ar_A_marginal)){
    fl_p_value = fl_P_A1_E1
    if (it_row >= 2){
      fl_p_value = fl_p_value + sum(ar_delta_E[1:(it_row-1)])
    }
    if (it_col >= 2){
      fl_p_value = fl_p_value + sum(ar_delta_A[1:(it_col-1)])
    }
    mt_P_A_E[it_row, it_col] = fl_p_value
  }
}
print(mt_P_A_E)
sum(mt_P_A_E)
rowSums(mt_P_A_E)
colSums(mt_P_A_E)

