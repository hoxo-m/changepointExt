#' @import changepoint
#' @import ggplot2 ggfortify

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @export
autoplot.cpt <- function(object, cpt.col = "red", cpt.width = 1, cpt.style = 1,
                         ...) {
  cpt_type <- cpttype(object)
  if (cpt_type %in% c("mean", "mean and variance")) {
    autoplot_cpt_mean(object, cpt.col, cpt.width, cpt.style, ...)
  } else if (cpt_type %in% c("variance")) {
    autoplot_cpt_var(object, cpt.col, cpt.width, cpt.style, ...)
  } else if (cpt_type %in% c("nonparametric (empirical_distribution)", "trend")) {
    stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
  } else {
    stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
  }
}

#' @importFrom zoo index
autoplot_cpt_mean <- function(cpt, cpt.col = "red", cpt.width = 1,
                              cpt.style = 1, ...) {
  ts <- data.set.ts(cpt)
  g <- autoplot(ts, ...) + xlab("Time") + ylab("data.set.ts(x)")

  cpt_inds <- cpts(cpt)
  if (length(cpt_inds) > 0) {
    if (1 %in% cpt_inds || length(ts) %in% cpt_inds) {
      stop("Changepoint indices contain the first or the last point.")
    }
    ts_points <- index(ts)
    ts_range <- range(ts_points)
    first_point <- ts_range[1]
    last_point <- ts_range[2]
    change_points <- c(first_point, ts_points[cpt_inds], last_point)
    means <- with(param.est(cpt), c(mean, mean[length(mean)]))
    step_df <- data.frame(x = change_points, y = means)
    g <- g +
      geom_step(data = step_df, aes_string(x = "x", y = "y"),
                color = cpt.col, size = cpt.width,
                linetype = cpt.style)
  }
  g
}

#' @importFrom zoo index
autoplot_cpt_var <- function(cpt, cpt.col = "red", cpt.width = 1,
                             cpt.style = 1, ...) {
  ts <- data.set.ts(cpt)
  g <- autoplot(ts, ...) + xlab("Time") + ylab("data.set.ts(x)")

  cpt_inds <- cpts(cpt)
  if (length(cpt_inds) > 0) {
    ts_points <- index(ts)
    change_points <- ts_points[cpt_inds]
    g <- g +
      geom_vline(xintercept = change_points, color = cpt.col, size = cpt.width,
                 linetype = cpt.style)
  }
  g
}
