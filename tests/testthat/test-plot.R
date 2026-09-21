# Unit tests for R/plot.R. All inputs are synthetic; no network. The grouped legend itself is tested in
# airquality.methods.

# all text labels drawn in a grob tree (legend titles, key labels, ...)
grob_texts <- function(grob) {
  texts <- if (inherits(grob, "text") || inherits(grob, "titleGrob")) as.character(grob$label) else character()
  children <- c(if (inherits(grob, "gTree")) grob$children else list(), if (inherits(grob, "gtable")) grob$grobs else list())
  c(texts, unlist(purrr::map(children, grob_texts)))
}

legend_texts <- function(plot) {
  gt <- withr::with_pdf(NULL, ggplot2::ggplotGrob(plot))
  boxes <- gt$grobs[grepl("^guide-box", gt$layout$name)]
  unlist(purrr::map(boxes, grob_texts))
}


# ---- ggplot_emissions() ------------------------------------------------------------

make_emission_plot_data <- function() {
  tibble::tibble(
    year = rep(2000:2001, each = 3),
    pollutant = "NOx", metric = "Jahresmenge", unit = "t/a",
    sector = rep(c("Verkehr", "Verkehr", "Land- und Forstw."), 2),
    subsector_new = rep(c("Strassenverkehr", "verschiedene", "verschiedene"), 2),
    order = rep(c(2, 3, 1), 2),
    col = rep(c("#1A1A1A", "#7F7F7F", "#3C096C"), 2),
    emission = c(10, 2, 3, 9, 2, 3)
  )
}

test_that("ggplot_emissions() shows sectors as legend blocks without pasting them to the subsectors", {
  plot <- ggplot_emissions(make_emission_plot_data())

  texts <- legend_texts(plot)
  expect_contains(texts, c("Verkehr", "Land- und Forstw.", "Strassenverkehr"))
  expect_equal(sum(texts == "verschiedene"), 2)
  expect_false(any(grepl(" / ", texts)))
})

test_that("ggplot_emissions() can move sectors, e.g. agriculture last", {
  plot <- ggplot_emissions(make_emission_plot_data(), sectors_last = "Land- und Forstw.")

  expect_equal(levels(plot$data$key)[3], "Land- und Forstw.::verschiedene")
})
