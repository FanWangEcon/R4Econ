## ----global_options, include = FALSE---------------------------------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# First make sure these are factors
tb_mtcars <- as_tibble(mtcars) %>%
  mutate(vs = as_factor(vs), am = as_factor(am))
# Second Label the Factors
am_levels <- c(auto_shift = "0", manual_shift = "1")
vs_levels <- c("vshaped engine" = "0", "straight engine" = "1")
tb_mtcars <- tb_mtcars %>%
  mutate(vs = fct_recode(vs, !!!vs_levels),
         am = fct_recode(am, !!!am_levels))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Graphing
plt_mtcars_scatter <-
  ggplot(tb_mtcars, aes(x=hp, y=qsec,
                        colour=am, shape=am, linetype=am)) +
  geom_smooth(se = FALSE, lwd = 1.5) + # Lwd = line width
  geom_point(size = 5, stroke = 2) + # stroke = point shape width
  facet_wrap(~ vs,
             scales = "free_x",
             nrow = 1, ncol = 2,
             labeller = label_wrap_gen(multi_line=FALSE))


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Color controls
ar_st_colors <- c("#33cc33", "#F8766D")
ar_st_colors_label <- c("auto", "manual")
fl_legend_color_symbol_size <- 5
st_leg_color_lab <- "Transmission"
# Shape controls
ar_it_shapes <- c(1, 5)
ar_st_shapes_label <- c("auto", "manual")
fl_legend_shape_symbol_size <- 5
st_leg_shape_lab <- "Transmission"
# Line-Type controls
ar_st_linetypes <- c('solid', 'dashed')
ar_st_linetypes_label <- c("auto", "manual")
fl_legend_linetype_symbol_size <- 5
st_leg_linetype_lab <- "Transmission"


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# x labeling and axis control
ar_st_x_labels <- c('50 hp', '150 hp', '250 hp', '350 hp')
ar_fl_x_breaks <- c(50, 150, 250, 350)
ar_fl_x_limits <- c(40, 360)
# y labeling and axis control
ar_st_y_labels <- c('15 QSEC', '18', '21', '24 QSEC')
ar_fl_y_breaks <- c(15, 18, 21, 24)
ar_fl_y_limits <- c(13.5, 25.5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Labeling
st_title <- paste0('How QSEC varies by Horse-power, by Engine and Transmission Types')
st_subtitle <- paste0('https://fanwangecon.github.io/',
                      'R4Econ/tabgraph/multiplot/htmlpdfr/fs_ggscatter_facet_wrap.html')
st_caption <- paste0('mtcars dataset, ',
                     'https://fanwangecon.github.io/R4Econ/')
st_x_label <- 'HP = Horse Power'
st_y_label <- 'QSEC = time for 1/4 Miles'


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# Add titles and labels
plt_mtcars_scatter <- plt_mtcars_scatter +
  labs(title = st_title, subtitle = st_subtitle,
       x = st_x_label, y = st_y_label, caption = st_caption)

# x and y-axis ticks controls
plt_mtcars_scatter <- plt_mtcars_scatter +
  scale_x_continuous(labels = ar_st_x_labels,
                     breaks = ar_fl_x_breaks,
                     limits = ar_fl_x_limits) +
  scale_y_continuous(labels = ar_st_y_labels,
                     breaks = ar_fl_y_breaks,
                     limits = ar_fl_y_limits)

# Color, shape and linetype controls
plt_mtcars_scatter <- plt_mtcars_scatter +
  scale_colour_manual(values=ar_st_colors, labels=ar_st_colors_label) +
  scale_shape_manual(values=ar_it_shapes, labels=ar_st_shapes_label) +
  scale_linetype_manual(values=ar_st_linetypes, labels=ar_st_linetypes_label)


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# has legend theme
theme_custom <- theme(
  text = element_text(size = 11),
  axis.text.y = element_text(angle = 90),
  legend.title = element_blank(),
  legend.position = c(0.35, 0.80),
  legend.key.width = unit(5, "line"),
  legend.background =
    element_rect(fill = "transparent", colour = "black", linetype='solid'))
# no legend theme (no y)
theme_custom_blank <- theme(
  text = element_text(size = 12),
  legend.title = element_blank(),
  legend.position = "none",
  axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank())


## --------------------------------------------------------------------------------------------------------------------------------------------------------
# replace the default labels for each legend segment
plt_mtcars_scatter <- plt_mtcars_scatter + theme_custom
# show
print(plt_mtcars_scatter)


## --------------------------------------------------------------------------------------------------------------------------------------------------------
for (it_subplot_as_own_vsr in c(1,2)) {

  if (it_subplot_as_own_vsr == 1) {
    theme_custom_use <- theme_custom
    # st_file_suffix <- '_haslegend'
    # it_width <- 100
  } else if (it_subplot_as_own_vsr == 2) {
    theme_custom_use <- theme_custom_blank
    # st_file_suffix <- '_nolegend'
    # it_width <- 88
  }

  # unique vs as matrix
  # ar_uniques <-sort(unique(tb_mtcars$vs))
  # mt_unique_vs <- matrix(data=ar_uniques, nrow=length(ar_uniques), ncol=1)
  mt_unique_vs <- tb_mtcars %>% group_by(vs) %>%
    summarize(mpg=mean(mpg)) %>% ungroup()
  # apply over
  ls_plots <- apply(mt_unique_vs, 1, function(ar_vs_cate_row) {
    # 1. Graph main
    plt_mtcars_scatter <-
      ggplot(tb_mtcars %>% filter(vs == ar_vs_cate_row[1]),
             aes(x=hp, y=qsec,
                 colour=am, shape=am, linetype=am)) +
      geom_smooth(se = FALSE, lwd = 1.5) + # Lwd = line width
      geom_point(size = 5, stroke = 2)

    # 2. Add titles and labels
    plt_mtcars_scatter <- plt_mtcars_scatter +
      labs(title = st_title, subtitle = st_subtitle,
           x = st_x_label, y = st_y_label, caption = st_caption)

    # 3. x and y ticks
    plt_mtcars_scatter <- plt_mtcars_scatter +
      scale_x_continuous(labels = ar_st_x_labels, breaks = ar_fl_x_breaks, limits = ar_fl_x_limits) +
      scale_y_continuous(labels = ar_st_y_labels, breaks = ar_fl_y_breaks, limits = ar_fl_y_limits)

    # 4. Color, shape and linetype controls
    plt_mtcars_scatter <- plt_mtcars_scatter +
      scale_colour_manual(values=ar_st_colors, labels=ar_st_colors_label) +
      scale_shape_manual(values=ar_it_shapes, labels=ar_st_shapes_label) +
      scale_linetype_manual(values=ar_st_linetypes, labels=ar_st_linetypes_label)

    # 5. replace the default labels for each legend segment
    plt_mtcars_scatter <- plt_mtcars_scatter + theme_custom_use
  })
  # show
  print(ls_plots)
}

