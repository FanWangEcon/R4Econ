## ----global_options, include = FALSE----------------------------------------------------------------------------------------------
try(source("../../.Rprofile"))


## ---------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(kableExtra)


## ---------------------------------------------------------------------------------------------------------------------------------
# Parameters
verbose <- TRUE
# Generate paths
spt_path_root <- file.path('C:', 'users', 'fan', 'R4Econ', fsep = .Platform$file.sep)
spt_path_in <- file.path(spt_path_root, 'table', 'bigtable', '_file', fsep = .Platform$file.sep)
spn_aod <- file.path(spt_path_in, 'lac_aod_children.csv', fsep = .Platform$file.sep)
spn_utci <- file.path(spt_path_in, 'lac_utci_children.csv', fsep = .Platform$file.sep)
spn_keys <- file.path(spt_path_in, 'country_code.csv', fsep = .Platform$file.sep)
# Load files
df_lac_aod_pollution <- read_csv(spn_aod) %>% 
  select(-order)
df_lac_utci_temperature <- read_csv(spn_utci) %>% 
  select(
    one_of(colnames(df_lac_aod_pollution)),
    pm10_grp_mean, pm10_overall_mean, pm10_grp_exc_burden
  ) %>% 
  rename(
    pm10_grp_mean_x = pm10_grp_mean, 
    pm10_grp_mean_y = pm10_overall_mean,
    relative_excess_burden = pm10_grp_exc_burden
  )
df_country_code <- read_csv(spn_keys)
# output tex file path
spn_tex_out <- file.path(
  spt_path_in, 'lac_climate_children_rank.tex', fsep = .Platform$file.sep)


## ---------------------------------------------------------------------------------------------------------------------------------
# Merge with key
df_lac_aod_pollution <- df_lac_aod_pollution %>% 
  left_join(df_country_code, by = "ISOCODE") 
# Merge with temperature information
df_lac_aod_pollution <- df_lac_aod_pollution %>% 
  left_join(df_lac_utci_temperature %>% 
    rename(utcige32c_age0t5 = pm10_grp_mean_x) %>%
    select(ISOCODE, utcige32c_age0t5), by = "ISOCODE") 
# Filter region to consider 
df_lac_aod_pollution_lac <- df_lac_aod_pollution %>%
  dplyr::filter(region_name == "Americas") %>%
  dplyr::filter(!(ISOCODE %in% c(
    "CAN", "USA", "CUB", "GRL"
    )))
# Select variables
df_lac_aod_pollution_lac <- df_lac_aod_pollution_lac %>% 
  rename(
    group_sorter_desc = 'Income group',
    aodavg_age0t5 = pm10_grp_mean_x
    ) %>%
  select(
    ISOCODE, country_name, group_sorter_desc, 
    aodavg_age0t5 , utcige32c_age0t5
  )
# Display 
str(df_lac_aod_pollution_lac)


## ---------------------------------------------------------------------------------------------------------------------------------
# Identify countries without country_name
ar_st_isocode_dependencies <- df_lac_aod_pollution_lac %>% 
  dplyr::filter(is.na(country_name)) %>% 
  pull(ISOCODE)
print(ar_st_isocode_dependencies)
# Add in names
# https://www.britannica.com/topic/list-of-countries-in-Latin-America-2061416
df_lac_aod_pollution_lac <- df_lac_aod_pollution_lac %>% 
  mutate(country_name = case_when(
    ISOCODE == "MTQ" ~ 'Martinique',
    ISOCODE == "MSR" ~ 'Montserrat',
    ISOCODE == "AIA" ~ 'Anguilla',
    ISOCODE == "BES" ~ 'St. Eustatius',
    ISOCODE == "GUF" ~ 'French Guiana',
    ISOCODE == "FLK" ~ 'Falkland Islands',
    TRUE ~ country_name
  ))
# Country name and ISO combine
df_lac_aod_pollution_lac <- df_lac_aod_pollution_lac %>% 
  mutate(country_name = paste0(
    country_name, " (", ISOCODE , ")"
    ))


## ---------------------------------------------------------------------------------------------------------------------------------
# Add in categorical name for locations without world bank income group designation
df_lac_aod_pollution_lac <- df_lac_aod_pollution_lac %>%
  mutate(group_sorter_desc= case_when(
    is.na(group_sorter_desc) ~ 'Without World Bank income group designation',
    TRUE ~ group_sorter_desc
  ))


## ---------------------------------------------------------------------------------------------------------------------------------
# basic chart with two lines
df_lac_aod_pollution_lac <- df_lac_aod_pollution_lac %>% 
  mutate(aodavg_age0t5 = aodavg_age0t5/1000) %>% 
  arrange(aodavg_age0t5) %>%
  mutate(aod_rank = row_number()) %>%
  arrange(utcige32c_age0t5) %>%
  mutate(utcige32c_rank = row_number())


## ---------------------------------------------------------------------------------------------------------------------------------
df_lac_aod_pollution_lac <- df_lac_aod_pollution_lac %>%
  mutate(group_sorter = case_when(
    group_sorter_desc == 'High income' ~ 1,
    group_sorter_desc == 'Upper middle income' ~ 2,
    group_sorter_desc == 'Lower middle income' ~ 3,
    group_sorter_desc == 'Without World Bank income group designation' ~ 4
  ))
# display
kable(df_lac_aod_pollution_lac, caption="Input data") %>% kable_styling_fc()


## ---------------------------------------------------------------------------------------------------------------------------------
# Sorted file
df_data_sorted <- df_lac_aod_pollution_lac %>%
  arrange(
    group_sorter, country_name,
  ) %>%
  group_by(group_sorter) %>%
  ungroup()
# Count by group
df_group_counts <- df_data_sorted %>%
  group_by(group_sorter_desc, group_sorter) %>%
  summarize(group_count = n()) %>%
  arrange(group_sorter) %>% ungroup() %>%
  mutate(group_count_start = cumsum(group_count) - group_count + 1) %>%
  mutate(group_count_end = cumsum(group_count)) %>%
  select(group_count_start, group_count_end, everything())
# display
kable(df_group_counts, caption="Group counter") %>% kable_styling_fc()


## ---------------------------------------------------------------------------------------------------------------------------------
# 4. Format columns, decimals, percentage signs, etc
df_data_formated <- df_data_sorted %>%
  arrange(group_sorter, country_name) %>%
  select(-group_sorter_desc, -group_sorter, -ISOCODE) %>%
  select(
    country_name, 
    aodavg_age0t5, aod_rank, 
    utcige32c_age0t5, utcige32c_rank) %>%
  mutate_at(
    vars(contains("utcige32c_age")),
    list(~ paste0(
      format(round(., 3) * 100,
        nsmall = 1,
        big.mark = ","
      ),
      "%"
    ))
  ) %>%
  mutate_at(
    vars(contains("aodavg_age")),
    list(~ ifelse(abs(.) <= 0.0001,
      paste0(
        formatC(. ,
          digits = 1,
          format = "fg",
          drop0trailing = T,
          flag = "#"
        )
      ),
      paste0(
        format(round(., 3),
          nsmall = 1,
          big.mark = ","
        )
      )
    ))
  )


## ---------------------------------------------------------------------------------------------------------------------------------
bl_main_save <- TRUE
ar_st_kableformat <- c("html", "latex")
for (st_kableformat in ar_st_kableformat) {
  # Column names
  ar_st_col_names <- c(
    "Country name",
    "Child (0-5) mean AOD exposure",
    "LAC child AOD rank",
    "Child (0-5) mean annual share of time over 32 UTCI degrees",
    "LAC child temperature rank"
  )
  # Define column groups, grouping the names above
  # =1/3/2 are number of columns group title covers
  ar_st_col_groups1 <- c(
    " " = 1,
    "Air pollution by aerosols (AOD between 0 and 1)" = 2,
    "At least strong heat stress exposure" = 2
  )
  # Second, we construct main table, and add styling.
  st_title <- paste(
    "LAC country child air pollution and heat exposure ranking in 2010"
    )
  bk_tab_a <- kbl(
    df_data_formated,
    format = st_kableformat,
    label = "tab:lac:aod:temp:rank",
    # escape = F,
    linesep = "",
    booktabs = T,
    longtable = T,
    align = "c",
    caption = st_title,
    col.names = ar_st_col_names
  ) %>%
    # see https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Bootstrap_table_classes
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = F, position = "left"
    )
  # Third, we add in column groups.
  bk_tab_a <- bk_tab_a %>%
    add_header_above(ar_st_col_groups1)
  # Fourth, we add in row groups.
  for (it_group in seq(1, dim(df_group_counts)[1])) {
    # Reion full name info
    st_loc <- as.character(df_group_counts[[it_group, "group_sorter_desc"]])
    # groups start and end
    it_group_count_start <- df_group_counts[[it_group, "group_count_start"]]
    it_group_count_end <- df_group_counts[[it_group, "group_count_end"]]
    # display text
    st_panel_letter <- base::LETTERS[it_group]
    # Heading group row, year
    st_panel_text <- paste0(
      "Panel ", st_panel_letter, ": ", st_loc 
    )
    # Add to table
    bk_tab_a <- bk_tab_a %>%
      pack_rows(
        st_panel_text, it_group_count_start, it_group_count_end,
        latex_gap_space = "0.25em",
        latex_align = "c",
        hline_after = TRUE
      )
  }
  # Fifth, column formatting.
  fl_width_country <- 6
  st_width_country <- paste0(fl_width_country, "cm")
  bk_tab_a <- bk_tab_a %>%
    column_spec(1, width = st_width_country) %>%
    column_spec(2:dim(df_data_formated)[2], width = "3cm")
  # Final adjustments
  # Headings on all pages, note use `sub` to replace first midrule
  st_headend <- paste0(
    "\\midrule\\endhead\n",
    "\\addlinespace[0.2em]\\hline\\addlinespace[0.2em]\n",
    "\\multicolumn{", dim(df_data_formated)[2], "}{r}{\\emph{Continued on next page}}\\\\\n",
    "\\endfoot\\endlastfoot"
  )
  bk_tab_a <- sub(bk_tab_a,
    pattern = "\\midrule", replacement = st_headend,
    fixed = TRUE
  )

  # country-names left-align
  bk_tab_a <- gsub(bk_tab_a,
    pattern = paste0("\\centering\\arraybackslash}p{", st_width_country, "}"),
    replacement = paste0("\\raggedright\\arraybackslash}p{", st_width_country, "}"),
    fixed = TRUE
  )
  bk_tab_a <- gsub(bk_tab_a,
    pattern = paste0("\\$\\textasciicircum{}\\{\\textbackslash{}circ\\}C\\$"),
    replacement = paste0("$^{\\circ}C$"),
    fixed = TRUE
  )
  bk_tab_a <- gsub(bk_tab_a,
    pattern = paste0("\\$\\textbackslash{}ge\\$"),
    replacement = paste0("$\\ge$"),
    fixed = TRUE
  )
  st_text <- ""
  bk_tab_a <- gsub(bk_tab_a,
    pattern = paste0("\\textbackslash{}", st_text, "\\"),
    replacement = paste0("\\", st_text),
    fixed = TRUE
  )
  # midrule replacing hline
  bk_tab_a <- gsub(bk_tab_a,
    pattern = "hline",
    replacement = "midrule", fixed = TRUE
  )

  # 6. Finally, save table content to file
  if (st_kableformat == "latex") {
    if (bl_main_save) {
      fileConn <- file(spn_tex_out)
      writeLines(bk_tab_a, fileConn)
      close(fileConn)
      if (verbose) {
        print(glue::glue("F-815346, S3"))
        print(glue::glue("Latex saved: {spn_tex_out}"))
      }
    }
  } else if (st_kableformat == "html") {
    bk_tab_a_html <- bk_tab_a
  }
}
bk_tab_a_html

