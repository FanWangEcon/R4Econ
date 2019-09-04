# Add a 45 degree line to figure and x=0 and y=0
geom_abline(intercept = 0, slope = 1, size=2, color='black', linetype=1, alpha=0.8) +
  geom_hline(yintercept = 0, size=1, color='black', linetype=2, alpha=0.5) +
  geom_vline(xintercept = 0, size=1, color='black', linetype=2, alpha=0.5)
