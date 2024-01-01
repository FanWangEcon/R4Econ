## ----global_options, include = FALSE--------------------------------------------------
try(source("../../.Rprofile"))


## -------------------------------------------------------------------------------------
# Get mtcars
df_mtcars <- mtcars

# case_when with mtcars
df_mtcars <- df_mtcars %>%
    mutate(
        mpg_qsec_am_grp =
            case_when(
                mpg < 16 ~ "< 16 MPG",
                qsec >= 20 ~ "> 16 MPG & qsec >= 20",
                am == 1 ~ "> 16 MPG & asec < 20 & manual",
                TRUE ~ "Others"
            )
    )


## -------------------------------------------------------------------------------------
# Labeling
st_title <- paste0("Use case_when To Generate ifelse Groupings")
st_subtitle <- paste0(
    "https://fanwangecon.github.io/",
    "R4Econ/amto/tibble/htmlpdfr/fs_tib_na.html"
)
st_caption <- paste0(
    "mtcars dataset, ",
    "https://fanwangecon.github.io/R4Econ/"
)
st_x_label <- "MPG = Miles per Gallon"
st_y_label <- "QSEC = time for 1/4 Miles"

# Graphing
plt_mtcars_casewhen_scatter <-
    ggplot(
        df_mtcars,
        aes(
            x = mpg, y = qsec,
            colour = mpg_qsec_am_grp,
            shape = mpg_qsec_am_grp
        )
    ) +
    geom_jitter(size = 3, width = 0.15) +
    labs(
        title = st_title, subtitle = st_subtitle,
        x = st_x_label, y = st_y_label, caption = st_caption
    ) +
    theme_bw()

# show
print(plt_mtcars_casewhen_scatter)


## -------------------------------------------------------------------------------------
# Generate a categorical variable
df_mtcars <- df_mtcars %>%
    mutate(gear_cate = case_when(
        gear == 3 ~ "gear is 3",
        gear == 4 ~ "gear is 4",
        gear == 5 & hp <= 110 ~ "gear 5 hp les sequal 110",
        gear == 5 & hp > 110 & hp <= 200 ~ "gear 5 hp 110 to 130",
        TRUE ~ "otherwise"
    ))
# Tabulate
df_mtcars_gear_tb <- df_mtcars %>% 
  group_by(gear_cate, gear) %>%
  tally() %>%
  spread(gear_cate, n)
# Display
st_title <- "Categorical from continuous with non-continuous values matching to same key"
df_mtcars_gear_tb %>% kable(caption = st_title) %>%
  kable_styling_fc()


## -------------------------------------------------------------------------------------
# Get mtcars
df_mtcars <- mtcars

# Make some values of mpg randomly NA
# the NA has to conform to the type of the remaining values for the new variable
# NA_real_, NA_character_, NA_integer_, NA_complex_
set.seed(2341)
df_mtcars <- df_mtcars %>%
    mutate(mpg_wth_NA1 = na_if(
        case_when(
            rnorm(n(), mean = 0, sd = 1) < 0 ~ -999,
            TRUE ~ mpg
        ),
        -999
    )) %>%
    mutate(mpg_wth_NA2 = case_when(
        rnorm(n(), mean = 0, sd = 1) < 0 ~ NA_real_,
        TRUE ~ mpg
    )) %>%
    mutate(mpg_wth_NA3 = case_when(
        rnorm(n(), mean = 0, sd = 1) < 0 ~ NA_character_,
        TRUE ~ "shock > 0 string"
    ))

# Generate New Variables based on if mpg_wth_NA is NA or not
# same variable as above, but now first a category based on if NA
# And we generate a fake string "NA" variable, this is not NA
# the String NA allows for it to be printed on figure
df_mtcars <- df_mtcars %>%
    mutate(
        group_with_na =
            case_when(
                is.na(mpg_wth_NA2) & is.na(mpg_wth_NA3) ~
                    "Rand String and Rand Numeric both NA",
                mpg < 16 ~ "< 16 MPG",
                qsec >= 20 ~ "> 16 MPG & qsec >= 20",
                am == 1 ~ "> 16 MPG & asec < 20 & manual",
                TRUE ~ "Fake String NA"
            )
    )

# show
kable(head(df_mtcars %>% select(starts_with("mpg")), 13)) %>%
    kable_styling_fc()
# # Setting to NA
# df.reg.use <- df.reg.guat %>% filter(!!sym(var.mth) != 0)
# df.reg.use.log <- df.reg.use
# df.reg.use.log[which(is.nan(df.reg.use$prot.imputed.log)),] = NA
# df.reg.use.log[which(df.reg.use$prot.imputed.log==Inf),] = NA
# df.reg.use.log[which(df.reg.use$prot.imputed.log==-Inf),] = NA
# df.reg.use.log <- df.reg.use.log %>% drop_na(prot.imputed.log)
# # df.reg.use.log$prot.imputed.log


## -------------------------------------------------------------------------------------
# Labeling
st_title <- paste0(
    "Use na_if and is.na to Generate and Distinguish NA Values\n",
    "NA_real_, NA_character_, NA_integer_, NA_complex_"
)
st_subtitle <- paste0(
    "https://fanwangecon.github.io/",
    "R4Econ/amto/tibble/htmlpdfr/fs_tib_na.html"
)
st_caption <- paste0(
    "mtcars dataset, ",
    "https://fanwangecon.github.io/R4Econ/"
)
st_x_label <- "MPG = Miles per Gallon"
st_y_label <- "QSEC = time for 1/4 Miles"

# Graphing
plt_mtcars_ifisna_scatter <-
    ggplot(
        df_mtcars,
        aes(
            x = mpg, y = qsec,
            colour = group_with_na,
            shape = group_with_na
        )
    ) +
    geom_jitter(size = 3, width = 0.15) +
    labs(
        title = st_title, subtitle = st_subtitle,
        x = st_x_label, y = st_y_label, caption = st_caption
    ) +
    theme_bw()

# show
print(plt_mtcars_ifisna_scatter)


## -------------------------------------------------------------------------------------
# Set tolerance
tol_lvl <- 1.5e-3
sd_lower_than_tol <- tol_lvl / 10
sd_higher_than_tol <- tol_lvl * 10

# larger SD
set.seed(123)
mt_runif_standard <- matrix(rnorm(10, mean = 0, sd = sd_higher_than_tol), nrow = 5, ncol = 2)

# small SD
set.seed(123)
mt_rnorm_small_sd <- matrix(rnorm(10, mean = 0, sd = sd_lower_than_tol), nrow = 5, ncol = 2)

# Generates Random Matirx
tb_rnorm_runif <- as_tibble(cbind(mt_rnorm_small_sd, mt_runif_standard))

# Are Variables the same, not for strict comparison
tb_rnorm_runif_approxi_same <- tb_rnorm_runif %>%
    mutate(
        V1_V2_ALMOST_SAME =
            case_when(
                isTRUE(all.equal(V1, V2, tolerance = tol_lvl)) ~
                    paste0("TOL=", sd_lower_than_tol, ", SAME ALMOST"),
                TRUE ~
                    paste0("TOL=", sd_lower_than_tol, ", NOT SAME ALMOST")
            )
    ) %>%
    mutate(
        V3_V4_ALMOST_SAME =
            case_when(
                isTRUE(all.equal(V3, V4, tolerance = tol_lvl)) ~
                    paste0("TOL=", sd_higher_than_tol, ", SAME ALMOST"),
                TRUE ~
                    paste0("TOL=", sd_higher_than_tol, ", NOT SAME ALMOST")
            )
    )

# Pring
kable(tb_rnorm_runif_approxi_same) %>% kable_styling_fc_wide()

