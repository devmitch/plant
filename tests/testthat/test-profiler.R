clean_search <- function() {
  defaults <- c(".GlobalEnv",
                paste0("package:", getOption("defaultPackages")),
                "Autoloads",
                "package:base")
  currentList <- search()
  deletes <- setdiff(currentList, defaults)
  for (entry in deletes)
    detach(entry, character.only = TRUE)
}

# function to knit directory
knit_dir <- function(dir) {
  # create the full path
  full_dir <- normalizePath(dir)
  
  # list the Rmd files with full names
  files <-
    list.files(full_dir, pattern = '*.Rmd$', full.names = TRUE)
  
  # render the files using all output formats in the YAML
  purrr::map(files, function(file) {
    # clean search list to avoid conflicts
    clean_search()
    
    # render the file using all
    # output formats in a new env
    rmarkdown::render(file,
                      output_format = "all",
                      envir = new.env())
  })
}


test_that("offspring arrival", {
  out_file <- tempfile("jointprof", fileext = ".pb.gz")
  jointprof::start_profiler(out_file)
  
  
  
  
  
  
  
  p0 <- scm_base_parameters("FF16")
  env <- make_environment("FF16")
  ctrl <- scm_base_control()
  
  # one species
  p1 <- expand_parameters(trait_matrix(0.0825, "lma"), p0, FF16_hyperpar,FALSE)
  
  p1$birth_rate <- 20
  out <- run_scm(p1, env, ctrl)
  expect_equal(out$offspring_production, 16.88946, tolerance=1e-5)
  expect_equal(out$ode_times[c(10, 100)], c(0.000070, 4.216055), tolerance=1e-5)
  
  # two species
  p2 <- expand_parameters(trait_matrix(0.2625, "lma"), p1, FF16_hyperpar, FALSE)
  p2$birth_rate <- c(11.99177, 16.51006)
  out <- run_scm(p2, env, ctrl)
  expect_equal(out$offspring_production, c(11.99529, 16.47519), tolerance=1e-5)
  expect_equal(length(out$ode_times), 297)
  
  
  
  profile_data <- jointprof::stop_profiler()
  pprof_file <- tempfile("jointprof", fileext = ".pb.gz")
  profile::write_pprof(profile_data, pprof_file)
  dir.create("jointprof_fig", recursive = TRUE, showWarnings = FALSE)
  svg_file <- "jointprof_fig/minimal.svg"
  
  system2(
    jointprof::find_pprof(),
    c(
      "-svg",
      "-nodefraction 0.01",
      "-output",
      shQuote(svg_file),
      shQuote(pprof_file)
    )
  )
  
  png_file <- "jointprof_fig/minimal.png"
  rsvg::rsvg_png(svg_file, png_file, width = 5000)
})
