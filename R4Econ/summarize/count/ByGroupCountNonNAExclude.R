
ff.by.group.count.nonna.exclude <- function (df, vars.group.by = c('S.country', 'svymthRound'),
                                             vars.arrange.by = c('S.country', 'svymthRound'),
                                             var.countobs = c('prot'),
                                             ls.fl.convert2na = c(0, -1),
                                             ls.fl.countkeep,
                                             st.var.countobs.n.suffix = '_nonna_n',
                                             st.var.countobs.NA = '_nonna',
                                             bl.print = TRUE,
                                             graph=TRUE) {
#' Count non-NA values of a variable within group, keep indi if obs count = some numbers
#'
#' Provides the square of the input
#' @param df dataframe
#' @param vars.group.by the variable to group by, individual id likely
#' @param vars.arrange.by arrange variables, does not impact outcomes, just for printing
#' @param var.countobs the variable for which to count non-NA occurances
#' @param ls.fl.convert2na valus in var.countobs to convert to NA, like 0,
#' so they would be exluded from counting.
#' @param ls.fl.countkeep list of N count where vars.group.by are kept, if missing keep all
#' @param st.var.countobs.NA suffix for variable that stores converted non-NA
#' @param st.var.countobs.n.suffix suffix for variable that stores count
#' @param bl.print if to print some statistics
#' @examples'
#' vars.group.by <- c(var.country, var.id)
#' vars.arrange.by <- c(var.country, var.id, var.mth)
#' var.countobs <- var.input
#' ls.fl.convert2na <- c(0, -1)
#' ls.fl.countkeep <- c(9)
#' st.var.countobs.n.suffix <- '_nonna_n'
#' st.var.countobs.NA <- '_nonna'
#' bl.print <- FALSE
#'
#' ls.count.df <- ff.by.group.count.nonna.exclude(
#'                     df, vars.group.by = vars.group.by, vars.arrange.by = vars.arrange.by,
#'                     var.countobs = var.countobs,
#'                     ls.fl.convert2na = ls.fl.convert2na, ls.fl.countkeep = ls.fl.countkeep,
#'                     st.var.countobs.n.suffix = st.var.countobs.n.suffix,
#'                     st.var.countobs.NA = st.var.countobs.NA,
#'                     bl.print = bl.print)
#'
#' df.counting <- ls.count.df$df_counting
#' df.countobs.nonNA.n <- ls.count.df$df_countobs_nonNA_n

  # Name variable that stores the number of non-NA within group counts.
  var.countobs.nonNA <- paste0(var.countobs, st.var.countobs.NA)
  var.countobs.nonNA.n <- paste0(var.countobs, st.var.countobs.n.suffix)

  # Ex-ante Review
  if (bl.print) {
      print(summary(df[[var.countobs]]))
  }

  # Convert values that we do not want to consider in counting as NA
  df <- df %>% mutate(!!(var.countobs.nonNA) := !!sym(var.countobs))
  for (fl.convert2na in ls.fl.convert2na) {
      df <- df %>% mutate(!!(var.countobs.nonNA) := na_if(!!sym(var.countobs.nonNA), fl.convert2na))
  }

  # Ex-post Review
  if (bl.print) {
      print(summary(df[[var.countobs.nonNA]]))
  }

  # Count the number of non-NA values
  df.counting <- df %>%
              drop_na(!!sym(var.countobs.nonNA)) %>%
              arrange(!!!syms(vars.arrange.by)) %>%
              group_by(!!!syms(vars.group.by)) %>%
              summarize(!!(var.countobs.nonNA.n) := n())
  if (bl.print) {
      print(head(df.counting, 5))
  }

  # Merge df.count back to main
  df.counting <- df %>% left_join(df.counting, by = vars.group.by)

  if (bl.print) {
    options(repr.matrix.max.rows=50, repr.matrix.max.cols=20)
    print(head(df.counting %>% filter(S.country == 'Cebu') %>%
          select(one_of(vars.group.by, vars.arrange.by, var.countobs, var.countobs.nonNA, var.countobs.nonNA.n)), 50))
  }

  # Group by could be a list, if group by has more than 1, then earlier groupbys are super-groups
  # Generally interested in count frequency within super-groups
  vars.count.groups <- c()
  if (length(vars.group.by) > 1) {
      # include non-last elements of group by
      vars.count.groups <- vars.group.by[-length(vars.group.by)]
  }
  vars.count.groups <- c(vars.count.groups, var.countobs.nonNA.n)
  var.unique.identifier <- vars.group.by[length(vars.group.by)]
  if (bl.print) {
    print(vars.count.groups)
    print(var.unique.identifier)
  }

  # Compute Unique Groups
  ls.df <- f.by.group.unique.obs(df.counting %>% ungroup() %>%
                        select(one_of(vars.group.by, vars.arrange.by, var.countobs, var.countobs.nonNA.n)),
                        vars.group = vars.count.groups, var.unique.identifier = var.unique.identifier, graph = FALSE)
  df.countobs.nonNA.n <- ls.df$df
  if (bl.print) {
    print(df.countobs.nonNA.n)
  }

  # keep subset that satisfy conditions
  if (!missing(ls.fl.countkeep)) {
      df.subset <- df.counting %>%
                  filter(!!sym(var.countobs.nonNA.n) %in% ls.fl.countkeep)

      # Compute Unique Groups
      ls.df.subset <- f.by.group.unique.obs(df.subset %>% ungroup() %>%
                            select(one_of(vars.group.by, vars.arrange.by, var.countobs, var.countobs.nonNA.n)),
                            vars.group = vars.count.groups, var.unique.identifier = var.unique.identifier, graph = FALSE)
      df.subset.countobs.nonNA.n <- ls.df.subset$df
      if (bl.print) {
        print(df.subset.countobs.nonNA.n)
      }
  }

  return(list(df_counting = df.counting,
              df_subset = df.subset,
              df_countobs_nonNA_n = df.countobs.nonNA.n,
              df_subset_countobs_nonNA_n = df.subset.countobs.nonNA.n))

}
