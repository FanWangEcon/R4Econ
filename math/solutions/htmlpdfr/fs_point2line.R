## ----global_options, include = FALSE-------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---- fig.width=5.25, fig.height=5.625-----------------------------------------------------------------
# Set random parameter Values for X1, Y1, and X2/Y2 ratio
set.seed(3)
fl_x1 <- runif(1) * 10
fl_y1 <- runif(1) * 10
fl_r <- runif(1) * 5

# Diaganol
fl_diag_slope <- -1 / fl_r
fl_diag_yintercept <- (fl_y1 * fl_r + fl_x1) / fl_r

# Closest point
fl_x2 <- (fl_y1 * fl_r + fl_x1) / (fl_r^2 + 1)
fl_y2 <- (fl_y1 * fl_r^2 + fl_x1 * fl_r) / (fl_r^2 + 1)

# Print state
print(paste("x1=", fl_x1, "x2=", fl_x2, "R=", fl_r, sep = " "))
print(paste("x2=", fl_x2, "y2=", fl_y2, sep = " "))

# X and y lims
ar_xylim <- c(-1, max(fl_x1, fl_y2) * 1.5)

# Visualize
par(mfrow = c(1, 1))
# Line through origin
curve(0 + fl_r * x, ar_xylim[1], ar_xylim[2],
  col = "black", lwd = 2, lty = 1,
  ylim = ar_xylim,
  ylab = "", xlab = ""
)
# Diaganol line
curve(fl_diag_yintercept + fl_diag_slope * x,
  add = TRUE,
  col = "blue", lwd = 2, lty = 2,
  ylim = ar_xylim,
  ylab = "", xlab = ""
)
# Point
points(fl_x1, fl_y1,
  add = TRUE,
  pch = 8, col = "red", cex = 3, lwd = 2,
  ylab = "", xlab = ""
)
points(fl_x2, fl_y2,
  add = TRUE,
  pch = 9, col = "darkgreen", cex = 3, lwd = 2,
  ylab = "", xlab = ""
)
# Origin lines
abline(
  v = 0, h = 0,
  col = "gray", lwd = 3, lty = 2
)

# Titles
title(
  main = paste0(
    "Line through origin and orthogonal line\n",
    "Find closest point along black line to red star"
  ),
  sub = paste0(
    "Black line goes through origin,",
    " blue line goes through (x1,y1) and (x2, y2)"
  ),
  xlab = "x values", ylab = "y values"
)

