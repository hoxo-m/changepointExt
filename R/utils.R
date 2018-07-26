#' @importFrom zoo zoo
data.set.ts2 <- function(cpt) {
  ts <- data.set.ts(cpt)
  ts_index <- attr(ts, "index")
  if (!is.null(ts_index)) {
    ts <- zoo(ts, order.by = ts_index)
  }
  ts
}
