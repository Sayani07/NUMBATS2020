## ---- theme-remark
theme_remark <- function() {
  theme_grey() +
    theme(
      axis.text = element_text(size = 24),
      strip.text = element_text(size = 24,
                                margin = margin()),
      axis.title = element_text(size = 24),
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      legend.position = "bottom",
      plot.title =  element_text(size = 24)
    ) 
}

## ---- theme-alldist
theme_alldist <- function() {
  theme_grey() +
    theme(
      axis.text = element_text(size = 20),
      strip.text = element_text(size = 20, margin = margin()),
      axis.title = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      plot.title =  element_text(size = 20)
    )
}
