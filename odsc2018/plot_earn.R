

#' generateBoxPlot
#' @description A function that allows you to generate a boxplot view of earnings data from UT system graduates.
#'
#' @param x Variable you'd like to have on the X axis
#' @param y Variable you'd like to have on the Y axis
#' @param facet If you'd like, a grouping variable
#' @param title If you'd like, a title for your plot
#'
#' @return A boxplot showing the distributions of earnings
#' @export
#'
#' @examples
#' \dontrun{
#' generateBoxPlot('year_postgrad'
#'                , 'p50_earnings'
#'                , 'deglevel'
#'                , "50th Pctile Earnings by Year Postgrad and Degree")
#' }
#'

generateBoxPlot <- function(x, y, facet = NULL, title = NULL){

  library(ggplot2)
  dataset <- feather::read_feather(system.file("data", "ut_grad_earn.feather", package = "earningsAnalysis"))

  if(!is.null(facet)){
  ggplot(dataset)+
    facet_grid(paste(". ~ ", facet))+
    theme_bw()+
    geom_boxplot(aes_string(x=x, y=y, group = x))+
      labs(x=x, y=y, title=title)
  } else {
    ggplot(dataset)+
      theme_bw()+
      geom_boxplot(aes_string(x=x, y=y, group = x))+
      labs(x=x, y=y, title=title)
  }

}

