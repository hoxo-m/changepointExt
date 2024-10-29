#' @export
combine_cpts <- function(..., operator = c("+", "*", "/")) {
  operator <- match.arg(operator)
  cpt_list <- list(...)
  stopifnot(length(cpt_list) >= 2L)
  stopifnot(operator == "/" && length(cpt_list) == 2L)
  labels <- names(cpt_list)
  result <- list(cpt_list = cpt_list, operator = operator, labels = labels)
  class(result) <- "cpt_combination"
  result
}
