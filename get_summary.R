
#' getEarningsSummary
#' @description A function that allows you to summarize the earnings data from UT system graduates.
#' @param ... Add as many summary variables as you'd like to use to group your table.
#'
#' @return dataframe showing summarized earnings by group.
#' @export
#'
#' @examples
#' \dontrun{
#' getEarningsSummary(deglevel)
#' }
#'

getEarningsSummary <- function(...){

  library(magrittr)
  library(rlang)

  dataset <- feather::read_feather(system.file("data", "ut_grad_earn.feather", package = "earningsAnalysis"))
  summaryvar <- quos(...)

  dataset_filter <- dataset %>%
    dplyr::group_by(!!!summaryvar) %>%
    dplyr::summarise(records = sum(cellcount, na.rm = T)
                     , mean_earn_25 = mean(p25_earnings, na.rm=T)
                     , mean_earn_50 = mean(p50_earnings, na.rm=T)
                     , mean_earn_75 = mean(p75_earnings, na.rm=T)) %>%
    dplyr::filter(records > 0)

  print(as.data.frame(dataset_filter))

}

