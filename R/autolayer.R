#' @import dplyr ggplot2
#' @importFrom tidyr fill replace_na
#' @importFrom zoo index
#' @export
autolayer.cpt_combination <- function(object, ...) {
  cpt_list <- object$cpt_list
  operator <- get(object$operator)

  df <- data.frame()
  for (i in seq_along(cpt_list)) {
    label <- object$labels[i]
    cpt <- cpt_list[[i]]
    means <- param.est(cpt)$mean
    cpt_inds <- c(1, cpts(cpt) + 1e-6 * i)
    change_points <- index(data.set.ts2(cpt))[cpt_inds]
    cpt_df <- data.frame(cpt_ind = cpt_inds, change_point = change_points, mean = means, label = label, stringsAsFactors = FALSE)
    if (nrow(df) == 0) {
      df <- cpt_df
    } else {
      df <- full_join(df, cpt_df, by = c("cpt_ind", "change_point"))
      df <- arrange(df, cpt_ind)
      df <- replace_na(df, list(label.x = "", label.y = ""))
      df <- fill(df, -cpt_ind)
      df <- mutate(df, mean = operator(mean.x, mean.y), label = paste0(label.x, label.y))
      df <- select(df, cpt_ind, change_point, mean, label)
    }
  }

  df <- mutate(df, last = lag(mean))
  df <- slice(df, -1)

  lay1 <- geom_segment(data = df, aes(x = change_point, xend = change_point,
                                      y = last, yend = mean, color = label),
                       arrow = arrow(), size = 1.5)
  m <- list(...)$m
  if (is.null(m)) m <- 1
  lay2 <- scale_color_manual(values = my_palette(length(cpt_list), m))
  list(lay1, lay2)
}

#' @importFrom grDevices hcl
default_palette <- function(n, l = 65) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = l, c = 100)[1:n]
}

my_palette <- function(n, m = 1) {
  if (m == 0) {
    default_palette(n)
  } else {
    default_palette(n + m)[-seq_len(m)]
  }
}
