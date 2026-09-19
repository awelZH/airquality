# Unit tests for the grouped legend in R/plot.R. All inputs are synthetic; no network.

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

make_grouped_data <- function() {
  tibble::tibble(
    year = rep(2000:2001, each = 5),
    sector = rep(c("Verkehr", "Verkehr", "Haushalte", "Industrie", "Industrie"), 2),
    subsector = rep(c("Strasse", "verschiedene", "Feuerungen", "Lösungsmittel", "verschiedene"), 2),
    order = rep(c(4, 5, 1, 2, 3), 2),
    emission = 1:10
  )
}

# ---- grouped_key() ---------------------------------------------------------------

test_that("grouped_key() makes one unique key per group and element, ordered by `order`", {
  data <- make_grouped_data()

  key <- grouped_key(data$sector, data$subsector, data$order)

  expect_s3_class(key, "factor")
  expect_equal(levels(key), c("Haushalte::Feuerungen", "Industrie::Lösungsmittel", "Industrie::verschiedene",
                              "Verkehr::Strasse", "Verkehr::verschiedene"))
  expect_equal(as.character(key[1]), "Verkehr::Strasse")
})

test_that("grouped_key() keeps the order of appearance without `order`", {
  key <- grouped_key(c("B", "A", "B"), c("x", "y", "z"))

  expect_equal(levels(key), c("B::x", "B::z", "A::y"))
})

test_that("grouped_key() moves groups with `group_order`, keeping the order within groups", {
  data <- make_grouped_data()

  key <- grouped_key(data$sector, data$subsector, data$order, group_order = c("Verkehr", "Haushalte", "Industrie"))

  expect_equal(levels(key), c("Verkehr::Strasse", "Verkehr::verschiedene", "Haushalte::Feuerungen",
                              "Industrie::Lösungsmittel", "Industrie::verschiedene"))
})

test_that("grouped_key() stops if a name contains the separator", {
  expect_error(grouped_key("A::B", "x"), "::")
})

# ---- split_grouped_keys() ----------------------------------------------------------

test_that("split_grouped_keys() keeps groups whole and balances the column heights", {
  # heights incl. one title line per group: A 4, B 2, C 2, D 4
  keys <- c("A::1", "A::2", "A::3", "B::1", "C::1", "D::1", "D::2", "D::3")

  columns <- split_grouped_keys(keys, ncol = 2)

  expect_equal(columns, list(c("A::1", "A::2", "A::3", "B::1"), c("C::1", "D::1", "D::2", "D::3")))
})

test_that("split_grouped_keys() returns one column for ncol = 1 and never an empty column", {
  keys <- c("A::1", "B::1")

  expect_equal(split_grouped_keys(keys, ncol = 1), list(keys))
  expect_equal(split_grouped_keys(keys, ncol = 3), list("A::1", "B::1"))
})

# ---- add_grouped_legend() ----------------------------------------------------------

make_grouped_plot <- function() {
  data <- make_grouped_data()
  data$key <- grouped_key(data$sector, data$subsector, data$order)
  colours <- c("Haushalte::Feuerungen" = "green", "Industrie::Lösungsmittel" = "blue", "Industrie::verschiedene" = "lightblue",
               "Verkehr::Strasse" = "black", "Verkehr::verschiedene" = "gray")
  ggplot2::ggplot(data, ggplot2::aes(year, emission, fill = key)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colours)
}

test_that("add_grouped_legend() shows group titles and the elements without the group", {
  plot <- add_grouped_legend(make_grouped_plot(), ncol = 2)

  texts <- legend_texts(plot)
  expect_s3_class(plot, "ggplot")
  expect_contains(texts, c("Haushalte", "Industrie", "Verkehr"))
  expect_contains(texts, c("Feuerungen", "Lösungsmittel", "Strasse"))
  expect_equal(sum(texts == "verschiedene"), 2)
  expect_false(any(grepl("::", texts)))
})

test_that("add_grouped_legend() draws the group titles like the legend labels (not bold, same size)", {
  plot <- add_grouped_legend(make_grouped_plot() + ggplot2::theme_minimal(base_size = 11), ncol = 2)

  gt <- withr::with_pdf(NULL, ggplot2::ggplotGrob(plot))
  text_grobs <- function(grob) {
    own <- if (inherits(grob, "text")) list(grob) else list()
    children <- c(if (inherits(grob, "gTree")) grob$children else list(), if (inherits(grob, "gtable")) grob$grobs else list())
    c(own, unlist(purrr::map(children, text_grobs), recursive = FALSE))
  }
  texts <- unlist(purrr::map(gt$grobs[grepl("^guide-box", gt$layout$name)], text_grobs), recursive = FALSE)
  style <- function(label) {
    grob <- purrr::detect(texts, \(g) identical(as.character(g$label), label))
    c(fontsize = grob$gp$fontsize, font = grob$gp$font %||% 1)
  }
  expect_equal(style("Haushalte"), style("Feuerungen"))
})

test_that("add_grouped_legend() draws one legend per column", {
  plot1 <- add_grouped_legend(make_grouped_plot(), ncol = 1)
  plot2 <- add_grouped_legend(make_grouped_plot(), ncol = 2)

  count_legends <- function(plot) {
    gt <- withr::with_pdf(NULL, ggplot2::ggplotGrob(plot))
    box <- gt$grobs[[which(gt$layout$name == "guide-box-right")]]
    sum(grepl("^guides", box$layout$name))
  }
  expect_equal(count_legends(plot1), 1)
  expect_equal(count_legends(plot2), 2)
})

test_that("add_grouped_legend() keeps the colours of the plot's scale", {
  plot <- add_grouped_legend(make_grouped_plot(), ncol = 2)

  built <- ggplot2::ggplot_build(plot)
  expect_setequal(unique(built$data[[1]]$fill), c("green", "blue", "lightblue", "black", "gray"))
})

test_that("add_grouped_legend() leaves no graphics device or Rplots.pdf behind", {
  withr::local_dir(withr::local_tempdir())
  grDevices::graphics.off() # devices left open by earlier tests would hide the problem

  plot <- add_grouped_legend(make_grouped_plot(), ncol = 2)

  expect_null(grDevices::dev.list())
  expect_false(file.exists("Rplots.pdf"))
})

test_that("add_grouped_legend() stops if the aesthetic has no scale", {
  expect_error(add_grouped_legend(make_grouped_plot(), aesthetic = "colour"), "colour")
})

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
