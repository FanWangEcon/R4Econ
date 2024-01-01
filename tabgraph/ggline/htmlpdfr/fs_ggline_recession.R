## ----global_options, include = FALSE--------------------------------------------------
try(source("../../.Rprofile"))


## -------------------------------------------------------------------------------------
# Load data, and treat index as "year", pretend data to be country-data
df_gdp <- as_tibble(attitude) %>%
    rowid_to_column(var = "year") %>%
    select(year, rating, complaints, learning) %>%
    rename(stats_usa = rating, stats_uk = learning) %>%
    pivot_longer(
        cols = starts_with("stats_"),
        names_to = c("country"),
        names_pattern = paste0("stats_(.*)"),
        values_to = "gdp"
    )

# Print
kable(df_gdp[1:10, ]) %>% kable_styling_fc()


## -------------------------------------------------------------------------------------
# Number of random starting index
it_start_idx <- min(df_gdp$year)
it_end_idx <- max(df_gdp$year)
it_startdraws_max <- 6
it_duramax <- 2
it_backward_win <- 0.3
it_forward_win <- 0.3

# Random seed
set.seed(1234)
# Draw random index between min and max
ar_it_start_idx <- sort(sample(
    seq(it_start_idx, it_end_idx),
    it_startdraws_max,
    replace = FALSE
))
# Draw random durations, replace = TRUE because can repeat
ar_it_duration <- sample(it_duramax, it_startdraws_max, replace = TRUE)

# Check space between starts
ar_it_startgap <- diff(ar_it_start_idx)
ar_it_dura_lenm1 <- ar_it_duration[1:(length(ar_it_duration) - 1)]
# Adjust durations
ar_it_dura_bd <- pmin(ar_it_startgap - 2, ar_it_dura_lenm1)
ar_it_duration[1:(length(ar_it_duration) - 1)] <- ar_it_dura_bd

# Drop consecutive starts
ar_bl_dura_nonneg <- which(ar_it_duration >= 0)
ar_it_start_idx <- ar_it_start_idx[ar_bl_dura_nonneg]
ar_it_duration <- ar_it_duration[ar_bl_dura_nonneg]

# Print
print(glue::glue(
    "random starts + duration: ",
    "{ar_it_start_idx} + {ar_it_duration}"
))


## -------------------------------------------------------------------------------------
# Offset by half of an integer
ar_fl_start_time <- ar_it_start_idx - 0.5
ar_fl_end_time <- ar_it_start_idx + ar_it_duration + 0.5

# Bound by min and max
ar_fl_end_time <- pmin(ar_fl_end_time, it_end_idx)
ar_fl_start_time <- pmax(ar_fl_start_time, it_start_idx)

# Backward window
ar_fl_end_time_win_backward <- ar_fl_start_time
ar_fl_start_time_win_backward <- pmax(
    ar_fl_start_time - it_backward_win, it_start_idx
)

# Forward window
ar_fl_end_time_win_forward <- pmin(
    ar_fl_end_time + it_forward_win, it_end_idx
)
ar_fl_start_time_win_forward <- ar_fl_end_time

# Print
print(glue::glue(
    "random start-time vs end-time: ",
    "{ar_fl_start_time} + {ar_fl_end_time}"
))


## -------------------------------------------------------------------------------------
# Variable names
# w1 = backward, w2 = main, w3 = forward, use w1, w2, w3 to facilate legend fill sorting
ar_st_varnames <- c(
    "recess_id",
    "year_start_w2", "year_end_w2",
    "year_start_w1", "year_end_w1",
    "year_start_w3", "year_end_w3"
)

# Recession index file
df_recession <- as_tibble(cbind(
    ar_fl_start_time, ar_fl_end_time,
    ar_fl_start_time_win_backward, ar_fl_end_time_win_backward,
    ar_fl_start_time_win_forward, ar_fl_end_time_win_forward
)) %>%
    rowid_to_column() %>%
    rename_all(~ c(ar_st_varnames))

# Reshape from Wide to Long
df_recession <- df_recession %>%
    pivot_longer(
        cols = starts_with("year"),
        names_to = c("time", "window"),
        names_pattern = "year_(.*)_(.*)",
        values_to = "year"
    ) %>%
    pivot_wider(
        id_cols = c("recess_id", "window"),
        names_from = time,
        names_prefix = "year_",
        values_from = year
    )


## -------------------------------------------------------------------------------------
# basic chart with two lines
pl_lines_basic <- ggplot() +
    geom_line(data = df_gdp, aes(
        x = year, y = gdp,
        color = country, linetype = country
    ), size = 1) +
    geom_rect(data = df_recession %>%
        filter(window == "w2"), aes(
        xmin = year_start, xmax = year_end,
        ymin = -Inf, ymax = Inf
    ), alpha = 0.6, fill = "gray") +
    labs(
        x = paste0("Years"),
        y = paste0("Ratings"),
        title = paste(
            "Main Title for this Figure over Countries (Shaded Recessions)",
            sep = " "
        ),
        subtitle = paste(
            "Subtitle for ratings changes across",
            "countries",
            sep = " "
        ),
        caption = paste(
            "Caption for our figure here ",
            "This is the next line ",
            "Another line",
            sep = ""
        )
    ) +
    theme_light() +
    facet_wrap(~country, ncol = 1)

# print figure
print(pl_lines_basic)


## -------------------------------------------------------------------------------------
# Window color
st_win_leg_title <- "Window"
st_win_color <- "gray"
st_win_label <- "Recession"
# basic chart with two lines
pl_lines <- ggplot() +
    geom_line(data = df_gdp, aes(
        x = year, y = gdp,
        color = country, linetype = country
    ), size = 1) +
    geom_rect(data = df_recession %>%
        filter(window == "w2"), aes(
        xmin = year_start, xmax = year_end,
        ymin = -Inf, ymax = Inf,
        fill = st_win_color
    ), alpha = 0.6) +
    theme_light()

# Titles
st_x <- "Years"
st_y <- "GDP"
st_subtitle <- "GDP changes across countries (shaded recessions)"
pl_lines <- pl_lines +
    labs(
        x = st_x,
        y = st_y,
        subtitle = st_subtitle
    )

# Figure improvements
# set shapes and colors
st_line_leg_title <- "Country"
ar_st_labels <- c(
    bquote("UK"),
    bquote("USA")
)

ar_st_colours <- c("#026aa3", "red")
ar_st_linetypes <- c("solid", "longdash")
pl_lines <- pl_lines +
    scale_colour_manual(values = ar_st_colours, labels = ar_st_labels) +
    scale_shape_discrete(labels = ar_st_labels) +
    scale_linetype_manual(values = ar_st_linetypes, labels = ar_st_labels) +
    scale_fill_manual(values = c(st_win_color), labels = c(st_win_label)) +
    labs(
        fill = st_win_leg_title,
        colour = st_line_leg_title, linetype = st_line_leg_title
    ) +
    theme(legend.key.width = unit(2.5, "line"))

# Axis
x_breaks <- seq(1, 30, length.out = 30)
x_labels <- paste0("Year ", x_breaks + 1990)

x_min <- 1
x_max <- 30

y_breaks <- seq(30, 90, length.out = 6)
y_labels <- paste0("y=", y_breaks)
y_min <- 30
y_max <- 90

pl_lines <- pl_lines +
    scale_x_continuous(
        labels = x_labels, breaks = x_breaks,
        limits = c(x_min, x_max)
    ) +
    theme(axis.text.x = element_text(
        # Adjust x-label angle
        angle = 90,
        # Adjust x-label distance to x-axis (up vs down)
        hjust = 0.4,
        # Adjust x-label left vs right wwith respect ot break point
        vjust = 0.5
    )) +
    scale_y_continuous(
        labels = y_labels, breaks = y_breaks,
        limits = c(y_min, y_max)
    )

# print figure
print(pl_lines)


## -------------------------------------------------------------------------------------
# Window color
st_win_leg_title <- "Window"
# basic chart with two lines
pl_lines <- ggplot() +
    geom_line(data = df_gdp, aes(
        x = year, y = gdp,
        color = country, linetype = country
    ), size = 1) +
    geom_rect(data = df_recession, aes(
        xmin = year_start, xmax = year_end,
        ymin = -Inf, ymax = Inf,
        fill = window
    ), alpha = 0.4) +
    theme_light()

# Titles
st_x <- "Years"
st_y <- "GDP"
st_subtitle <- "GDP changes across countries (shaded recessions, with pre and post)"
pl_lines <- pl_lines +
    labs(
        x = st_x,
        y = st_y,
        subtitle = st_subtitle
    )

# Figure improvements
# fill label and colors
ar_st_win_color <- c("darkgreen", "black", "darkgreen")
ar_st_win_label <- c("Backward", "Recession", "Forward")
# set shapes and colors
st_line_leg_title <- "Country"
ar_st_labels <- c(
    bquote("UK"),
    bquote("USA")
)

ar_st_colours <- c("#026aa3", "red")
ar_st_linetypes <- c("solid", "longdash")
pl_lines <- pl_lines +
    scale_colour_manual(values = ar_st_colours, labels = ar_st_labels) +
    scale_shape_discrete(labels = ar_st_labels) +
    scale_linetype_manual(values = ar_st_linetypes, labels = ar_st_labels) +
    scale_fill_manual(values = c(ar_st_win_color), labels = c(ar_st_win_label)) +
    labs(
        fill = st_win_leg_title,
        colour = st_line_leg_title, linetype = st_line_leg_title
    ) +
    theme(legend.key.width = unit(2.5, "line"))

# Axis
x_breaks <- seq(1, 30, length.out = 30)
x_labels <- paste0("Year ", x_breaks + 1990)

x_min <- 1
x_max <- 30

y_breaks <- seq(30, 90, length.out = 6)
y_labels <- paste0("y=", y_breaks)
y_min <- 30
y_max <- 90

pl_lines <- pl_lines +
    scale_x_continuous(
        labels = x_labels, breaks = x_breaks,
        limits = c(x_min, x_max)
    ) +
    theme(axis.text.x = element_text(
        # Adjust x-label angle
        angle = 90,
        # Adjust x-label distance to x-axis (up vs down)
        hjust = 0.4,
        # Adjust x-label left vs right wwith respect ot break point
        vjust = 0.5
    )) +
    scale_y_continuous(
        labels = y_labels, breaks = y_breaks,
        limits = c(y_min, y_max)
    )

# print figure
print(pl_lines)

