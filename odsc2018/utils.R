getVarChoices <- function(){

  dataset <- feather::read_feather(system.file("data", "ut_grad_earn.feather", package = "earningsAnalysis"))

  numerics <- names(dataset[ , purrr::map_lgl(dataset, is.numeric)])
  cats <- names(dataset[ , purrr::map_lgl(dataset, is.character)])

  print(paste("The numeric columns are called", paste0(numerics, collapse = ", " )))
  print(paste("The character columns are called", paste0(cats, collapse = ", ")))

}
