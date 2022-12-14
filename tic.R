# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks(
  args = "--no-vignettes",
  build_args = "--no-build-vignettes",
  error_on = "error"
)

get_stage("install") %>%
  #add_code_step(remotes::install_github(
  #  repo = "kapsner/mlexperiments",
  #  upgrade = "always",
  #  dependencies = c("Depends", "Imports", "Suggests"),
  #  force = TRUE
  #)) %>%
  add_code_step(remotes::install_cran(c("ParBayesianOptimization", "mlr3measures"))) %>%
  add_code_step(devtools::install(".", upgrade = "always"))
