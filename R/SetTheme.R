#' @title Set theme for plots by ggplot2.
#' @noRd
#' 
SetTheme <- function () {
  theme_bw() +
    theme(
      axis.text = element_text(size=rel(.9)),
      axis.title = element_text(size=rel(1)),
      strip.text = element_text(size=rel(1)),
      strip.background = element_rect(fill = "grey95"),
      panel.grid = element_line(size = 0.3, linetype = 2),
      plot.margin = margin(c(1,12,1,1), unit = "pt"),
      legend.title = element_text(size=rel(.9)),
      legend.text = element_text(size=rel(.85)),
      legend.position = 'top'
    )
}