## ----global_options, include = FALSE-------------------------------------------------------------------
try(source("../../.Rprofile"))


## ------------------------------------------------------------------------------------------------------
# positive value exponents
ar_exponents_posv <- c(0.05, 0.5, 1, 1.5)
# positive and negative values of the base
ar_baseval_pos <- seq(1e-10, 1.5, length.out=1000)
# base to power
mt_x2a_val <- matrix(data=NA, nrow=length(ar_exponents_posv), ncol=length(ar_baseval_pos))
# Generate values
it_row_ctr <- 0
for (fl_exponents_posv in ar_exponents_posv) {
  it_row_ctr <- it_row_ctr + 1
  mt_x2a_val[it_row_ctr,] <- ar_baseval_pos^fl_exponents_posv
}


## ------------------------------------------------------------------------------------------------------
# x and bounds
ar_xlim = c(min(ar_baseval_pos), max(ar_baseval_pos))
ar_ylim = c(0, 1.5)
# function line
st_line_1_y_legend <- paste0('x^', ar_exponents_posv[1])
st_line_2_y_legend <- paste0('x^', ar_exponents_posv[2])
st_line_3_y_legend <- paste0('x^', ar_exponents_posv[3])
st_line_4_y_legend <- paste0('x^', ar_exponents_posv[4])
# Color and line
st_point_1_pch <- 10
st_point_1_cex <- 2
ar_colors <- c('blue', 'red', 'black', 'orange')
ar_ltys <- c('solid', 'dashed', 'dotted', 'dotdash')
# Graph and combine
for (it_graph in c(1,2,3,4)) {
  if (it_graph != 1) {
    par(new=T)
  }
  ar_y_current <- mt_x2a_val[it_graph, ]
  plot(ar_baseval_pos, ar_y_current, type="l",
       col = ar_colors[it_graph], lty = ar_ltys[it_graph],
       pch = 10, cex = 2, xlim = ar_xlim, ylim = ar_ylim, panel.first = grid(),
       ylab = '', xlab = '', yaxt='n', xaxt='n', ann=FALSE)
  plot_line <- recordPlot()
}
# CEX sizing Contorl Titling and Legend Sizes
fl_ces_fig_reg = 1
fl_ces_fig_small = 0.75
# R Legend
st_title <- paste0('Positive Exponential Graphing')
st_subtitle <- paste0('https://fanwangecon.github.io/',
                      'R4Econ/math/solutions/htmlpdfr/fs_inequality.html')
st_x_label <- 'x'
st_y_label <- 'x^exponent'
title(main = st_title, sub = st_subtitle, xlab = st_x_label, ylab = st_y_label,
      cex.lab=fl_ces_fig_reg,
      cex.main=fl_ces_fig_reg,
      cex.sub=fl_ces_fig_small)
axis(1, cex.axis=fl_ces_fig_reg)
axis(2, cex.axis=fl_ces_fig_reg)
grid()
# Legend sizing CEX
legend("bottomright",
       inset=c(0,0),
       xpd=TRUE,
       c(st_line_1_y_legend, st_line_2_y_legend, st_line_3_y_legend, st_line_4_y_legend),
       col = c(ar_colors[1], ar_colors[2], ar_colors[3], ar_colors[4]),
       cex = fl_ces_fig_small,
       lty = c(ar_ltys[1], ar_ltys[2], ar_ltys[3], ar_ltys[4]),
       title = 'Legends',
       y.intersp=2)


## ------------------------------------------------------------------------------------------------------
# positive value exponents
ar_exponents_posv <- -c(0.05, 0.5, 1, 1.5)
# positive and negative values of the base
ar_baseval_pos <- seq(1e-10, 1.5, length.out=1000)
# base to power
mt_x2a_val <- matrix(data=NA, nrow=length(ar_exponents_posv), ncol=length(ar_baseval_pos))
# Generate values
it_row_ctr <- 0
for (fl_exponents_posv in ar_exponents_posv) {
  it_row_ctr <- it_row_ctr + 1
  mt_x2a_val[it_row_ctr,] <- ar_baseval_pos^fl_exponents_posv
}


## ------------------------------------------------------------------------------------------------------
# x and bounds
ar_xlim = c(min(ar_baseval_pos), max(ar_baseval_pos))
ar_ylim = c(0, 3)
# function line
st_line_1_y_legend <- paste0('x^', ar_exponents_posv[1])
st_line_2_y_legend <- paste0('x^', ar_exponents_posv[2])
st_line_3_y_legend <- paste0('x^', ar_exponents_posv[3])
st_line_4_y_legend <- paste0('x^', ar_exponents_posv[4])
# Color and line
st_point_1_pch <- 10
st_point_1_cex <- 2
ar_colors <- c('blue', 'red', 'black', 'orange')
ar_ltys <- c('solid', 'dashed', 'dotted', 'dotdash')
# Graph and combine
for (it_graph in c(1,2,3,4)) {
  if (it_graph != 1) {
    par(new=T)
  }
  ar_y_current <- mt_x2a_val[it_graph, ]
  plot(ar_baseval_pos, ar_y_current, type="l",
       col = ar_colors[it_graph], lty = ar_ltys[it_graph],
       pch = 10, cex = 2, xlim = ar_xlim, ylim = ar_ylim, panel.first = grid(),
       ylab = '', xlab = '', yaxt='n', xaxt='n', ann=FALSE)
  plot_line <- recordPlot()
}
# CEX sizing Contorl Titling and Legend Sizes
fl_ces_fig_reg = 1
fl_ces_fig_small = 0.75
# R Legend
st_title <- paste0('Negative Exponential Graphing')
st_subtitle <- paste0('https://fanwangecon.github.io/',
                      'R4Econ/math/solutions/htmlpdfr/fs_inequality.html')
st_x_label <- 'x'
st_y_label <- 'x^exponent'
title(main = st_title, sub = st_subtitle, xlab = st_x_label, ylab = st_y_label,
      cex.lab=fl_ces_fig_reg,
      cex.main=fl_ces_fig_reg,
      cex.sub=fl_ces_fig_small)
axis(1, cex.axis=fl_ces_fig_reg)
axis(2, cex.axis=fl_ces_fig_reg)
grid()
# Legend sizing CEX
legend("topright",
       inset=c(0,0),
       xpd=TRUE,
       c(st_line_1_y_legend, st_line_2_y_legend, st_line_3_y_legend, st_line_4_y_legend),
       col = c(ar_colors[1], ar_colors[2], ar_colors[3], ar_colors[4]),
       cex = fl_ces_fig_small,
       lty = c(ar_ltys[1], ar_ltys[2], ar_ltys[3], ar_ltys[4]),
       title = 'Legends',
       y.intersp=2)

