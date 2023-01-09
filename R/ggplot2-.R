wrap_by <- function(...) {
  ggplot2::facet_wrap(ggplot2::vars(...), labeller = ggplot2::label_both)
}
