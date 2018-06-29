#' @export
autolayer <- function(object, ...) {
  UseMethod("autolayer")
}

#' @import dplyr
#' @importFrom tidyr fill
#' @importFrom zoo index
#' @export
autolayer.cpt_combination <- function(object, ...) {
  cpt_list <- object$cpt_list

  df <- data.frame()
  for (cpt in cpt_list) {
    means <- param.est(cpt)$mean
    cpt_inds <- c(1, cpts(cpt))
    change_points <- index(data.set.ts(cpt))[cpt_inds]
    cpt_df <- data.frame(change_point = change_points, mean = means)
    if (nrow(df) == 0) {
      df <- cpt_df
    } else {
      df <- full_join(df, cpt_df, by="change_point")
      df <- fill(df, -change_point)
      df <- mutate(df, mean = mean.x + mean.y)
      df <- select(df, change_point, mean)
    }
  }

  cpt_inds_list <- lapply(cpt_list, function(x) cpts(x))
  cpt_inds <- cpt_inds_list %>% unlist %>% unique %>% sort
  labels <- rep(NA_character_, length(cpt_inds))
  names(labels) <- paste0("N", cpt_inds)
  for (i in seq_along(cpt_inds_list)) {
    labels[paste0("N", cpt_inds_list[i])] <- object$labels[i]
  }

  df <- mutate(df, last = lag(mean))
  df <- slice(df, -1)
  df <- mutate(df, label = labels)

  lay1 <- geom_segment(data = df, aes(x = change_point, xend = change_point,
                                      y = last, yend = mean, color = label),
                       arrow = arrow(), size = 1.5)
  lay2 <- scale_color_manual(values = my_palette(length(cpt_list)))
  list(lay1, lay2)
}

my_palette <- function(n) {
  hues <- seq(15, 375, length = n + 2)
  hcl(h = hues, l = 65, c = 100)[2:(n+1)]
}
