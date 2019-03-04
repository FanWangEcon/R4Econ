# show.unique.values(tibble(names=names(df_avg_input)), 'names', FALSE, 25, 5)
# A function that shows Unique Values for Categorical Variables in a Table format
show.unique.values <- function(df, cate.var.str,
                               count_n = TRUE,
                               lvl_str_max_len=15,
                               matrix_col_n=8){

    # Unique Categories
    unique.cates <- df %>%
        group_by(!!sym(cate.var.str)) %>%
        summarise(freq = n())

    if (count_n) {
      unique.cates <- unique.cates %>%
          mutate(distinct_N = paste0(!!sym(cate.var.str), ' (n=', freq, ')')) %>%
          select(distinct_N)
    } else {
      unique.cates <- unique.cates %>%
          mutate(distinct_N = paste0(!!sym(cate.var.str))) %>%
          select(distinct_N)
    }

    # At most 10 columns
    unique.count <- dim(unique.cates)[1]
    col.count <- min(ceiling(sqrt(unique.count)), matrix_col_n)
    row.count <- ceiling(unique.count/col.count)

    # Generate Table to Fill in
    expand.length = row.count*col.count
    unique.cates.expand <- vector(mode = "character", length = expand.length)

    # Unique Categories and Counts
    unique.cates.shorter <- substring(t(unique.cates), first = 1, last = lvl_str_max_len)
    unique.cates.expand[0:unique.count] <- unique.cates.shorter

    # Reshape
    dim(unique.cates.expand) <- c(row.count, col.count)

    # Show
    title <- sprintf("From Dataset: %s, %d unique Levels for: %s",
                     deparse(substitute(df)), unique.count, cate.var.str)
    return(list(title=title,
           levels=unique.cates.expand))
}
