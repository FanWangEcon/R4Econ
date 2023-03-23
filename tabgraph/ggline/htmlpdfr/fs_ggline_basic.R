## ----global_options, include = FALSE-----------------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------------
# Load data, and treat index as "year"
# pretend data to be country-data
df_attitude <- as_tibble(attitude) %>%
  rowid_to_column(var = "year") %>% 
  select(year, rating, complaints, learning) %>% 
  rename(stats_usa = rating, 
         stats_canada = complaints, 
         stats_uk = learning)

# Wide to Long
df_attitude <- df_attitude %>%
  pivot_longer(cols = starts_with('stats_'),
               names_to = c('country'),
               names_pattern = paste0("stats_(.*)"),
               values_to = "rating")

# Print 
kable(df_attitude[1:10,]) %>% kable_styling_fc()


## ----------------------------------------------------------------------------------------------------------
# basic chart with two lines
pl_lines_basic <- df_attitude %>% 
  ggplot(aes(x=year, y=rating, 
    color=country, linetype=country)) + 
  geom_line(size = 1) + 
  labs(x = paste0("Years"),
       y = paste0("Ratings"),
       title = paste(
        "Main Title for this Figure over",
        "Countries", sep=" "),
       subtitle = paste(
        "Subtitle for ratings changes across",
        "countries", sep=" "),
       caption = paste(
        "Caption for our figure here ",
        "This is the next line ", 
        "Another line", sep=""))
  
# print figure
print(pl_lines_basic)


## ----------------------------------------------------------------------------------------------------------
# basic chart with two lines
pl_lines <- df_attitude %>% 
  ggplot(aes(x=year, y=rating, 
    color=country, linetype=country, shape=country)) + 
  geom_line(size=1)

# Titles
st_x = "Years"
st_y = "Ratings"
st_subtitle = "Ratings changes across countries"
pl_lines <- pl_lines +
  labs(
    x = st_x,
    y = st_y,
    subtitle = st_subtitle)
  
# Figure improvements
# set shapes and colors
ar_st_labels <- c(
  bquote("Canada"),
  bquote("UK"),
  bquote("USA"))

ar_st_colours <- c("#85ccff", "#026aa3", "red")
ar_st_linetypes <- c("solid", "dashed", "longdash")
pl_lines <- pl_lines +
  scale_colour_manual(values = ar_st_colours, labels = ar_st_labels) +
  scale_shape_discrete(labels = ar_st_labels) +
  scale_linetype_manual(values = ar_st_linetypes, labels = ar_st_labels)

# Axis
x_labels <- c("Yr 1990", "Year 2000", "Y 2010", "y-2015", "2020 (year)")
x_breaks <- c(0, 10, 20, 25, 30)
x_min <- 0
x_max <- 30

y_breaks <- seq(30, 90, length.out=6)
y_labels <- paste0('y=', y_breaks)
y_min <- 30
y_max <- 90

pl_lines <- pl_lines + 
  scale_x_continuous(
    labels = x_labels, breaks = x_breaks,
    limits = c(x_min, x_max)
  ) +
  theme(axis.text.x = element_text(
    # Adjust x-label angle
    angle = 45, 
    # Adjust x-label distance to x-axis (up vs down)
    hjust = 0.4, 
    # Adjust x-label left vs right wwith respect ot break point
    vjust = 0.5)) + 
  scale_y_continuous(
    labels = y_labels, breaks = y_breaks,
    limits = c(y_min, y_max)
  )

# print figure
print(pl_lines)

