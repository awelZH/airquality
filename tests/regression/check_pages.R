# Run the R code of the Quarto pages without rendering: a quick check (about 2 min) that every page finds its
# plots, before the full render (about 7 min).
#
# For each page, the code chunks (knitr::purl()) and then the inline expressions (`r ...`) run in their own
# environment from docs/, drawing into a temporary directory; nothing is written to docs/. A page fails on the first
# error. Chunks with `eval: false` are skipped by purl(); printed markdown goes to a temporary
# file.
#
# run from the project root in a fresh R session, e.g.
#   Rscript -e 'source("tests/regression/check_pages.R"); check_pages()'

check_pages <- function(pages = list.files("docs", pattern = "[.]qmd$")) {
  out <- withr::local_tempdir()
  # figures of knitr::knit() calls in inline code go to the temporary directory, not to docs/figure/
  old_fig_path <- knitr::opts_chunk$get("fig.path")
  knitr::opts_chunk$set(fig.path = file.path(out, "figure/"))
  on.exit(knitr::opts_chunk$set(fig.path = old_fig_path), add = TRUE)
  results <- purrr::map_chr(rlang::set_names(pages), \(page) {
    withr::with_dir("docs", {
      script <- file.path(out, sub("[.]qmd$", ".R", page))
      knitr::purl(page, output = script, quiet = TRUE, documentation = 0)
      inline <- unlist(regmatches(readLines(page, encoding = "UTF-8"), gregexpr("`r [^`]+`", readLines(page, encoding = "UTF-8"))))
      env <- new.env(parent = globalenv())
      tryCatch({
        sink(tempfile())
        on.exit(sink(), add = TRUE)
        # ragg device into the temporary directory (a pdf device does not know Arial of theme_ts)
        ragg::agg_png(file.path(out, paste0(sub("[.]qmd$", "", page), "-%03d.png")))
        on.exit(grDevices::dev.off(), add = TRUE)
        sys.source(script, envir = env, keep.source = FALSE)
        for (code in inline) eval(parse(text = sub("^`r (.*)`$", "\\1", code)), envir = env)
        "OK"
      }, error = \(e) conditionMessage(e))
    })
  })

  failed <- results[results != "OK"]
  cli::cli_inform(c(
    "{length(pages) - length(failed)} of {length(pages)} page{?s} OK",
    if (length(failed) > 0) purrr::set_names(paste0(names(failed), ": ", failed), rep("x", length(failed)))
  ))
  invisible(results)
}
