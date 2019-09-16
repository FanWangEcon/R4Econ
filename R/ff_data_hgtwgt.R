#' Test Dataset Panel Height and Weight, Guatemala and Philippines, scrambled data, not actual.
#'
#' Data from INCAP and CEBU, used in various projects, \url{http://fanwangecon.github.io}
#'
#' @docType data
#'
#' @usage data(df_hgt_wgt)
#'
#' @format csv
#'
#' @keywords datasets
#'
#' @references “Early life height and weight production functions with endogenous energy and protein inputs”, September 2016, 65-81, Economics & Human Biology, 22, Esteban Puentes, Fan Wang, Jere R. Behrman, Flavio Cunha, John Hoddinott, John A. Maluccio, Linda S. Adair, Judith Borja and Reynaldo Martorell
#'
#' @source \url{https://www.sciencedirect.com/science/article/pii/S1570677X16300107}
#'
#' @examples
#' data(df_hgt_wgt)
#' times <- attr(df_hgt_wgt, "hgt")
#' phe <- df_hgt_wgt$wgt
"df_hgt_wgt"
