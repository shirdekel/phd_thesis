#' @title Plot prospect theory value function

#' @return
#' @author Shir Dekel
#' @export
plot_prospect_theory <- function() {
  lower_bound <- -2.5
  upper_bound <- 2.5

  data <-
    tibble(
      x = seq(from = lower_bound, to = upper_bound, by = 0.01),
      y = prospect_theory_value(x,
        alpha = 0.88,
        lambda = 2.25
      )
    ) %>%
    filter(between(y, lower_bound, upper_bound))

  date_line <-
    data %>%
    filter(x %in% c(-1, 1))

  data %>%
    ggplot(aes(x = x, y = y)) +
    xlim(lower_bound, upper_bound) +
    ylim(lower_bound, upper_bound) +
    geom_line() +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    # For some reason needed to make the linetype work
    scale_linetype_identity() +
    geom_segment(aes(x = 0, y = y, xend = x, yend = y, linetype = "dotted"),
      data = date_line
    ) +
    geom_segment(aes(x = x, y = 0, xend = x, yend = y, linetype = "dotted"),
      data = date_line
    ) +
    geom_text(aes(x = 0, y = y, label = round(y, 2)),
      data = date_line,
      nudge_x = c(0.3, -0.12),
      size = 5,
    ) +
    geom_text(aes(x = x, y = 0, label = x),
      data = date_line,
      nudge_y = c(0.12, -0.12),
      size = 5,
    ) +
    theme_apa() +
    labs(
      x = "Change from reference point",
      y = "Utility"
    ) +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )
}
