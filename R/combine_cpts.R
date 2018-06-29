#' @export
combine_cpts <- function(..., operator = "+") {
  cpt_list <- list(...)
  labels <- names(cpt_list)
  result <- list(cpt_list = cpt_list, operator = operator, labels = labels)
  class(result) <- "cpt_combination"
  result
}
