#' Create a new simulation object
#'
#' @description Create a new simulation object. This is typically the first
#'     function to be called when running a simulation using \pkg{SimEngine}. Most
#'     other \pkg{SimEngine} functions take a simulation object as their first
#'     argument.
#' @return A simulation object, of class \code{sim_obj}
#' @seealso
#' Visit \url{https://avi-kenny.github.io/SimEngine/} for more information on how to
#'     use the \pkg{SimEngine} simulation framework.
#' @examples
#' sim <- new_sim()
#' sim
#' @export
new_sim <- function() {

  # First check if dependencies are installed
  for (pkg in c("magrittr", "dplyr", "parallel", "pbapply", "data.table",
                "rlang", "methods")) {
    if (!requireNamespace(pkg, quietly=TRUE)) {
      stop(paste0(
        "You need to install the package '", pkg, "' for SimEngine to work."
      ))
    }
  }
  rm(pkg)

  # Create "blank" simulation object
  ..seed <- as.integer(1e9*runif(1))
  ..e <- .GlobalEnv
  ...sim <- list(
    config = list(
      num_sim = 10,
      datasets = "many",
      parallel = "none",
      n_cores = parallel::detectCores() - 1,
      packages = NULL,
      stop_at_error = FALSE,
      seed = ..seed,
      progress_bar = TRUE
    ),
    levels = list("no levels"=TRUE),
    levels_grid = data.frame(level_id=1),
    results = "Simulation has not been run yet.",
    results_complex = NA,
    errors = "Simulation has not been run yet.",
    warnings = "Simulation has not been run yet.",
    internals = list(
      levels_types = FALSE,
      levels_shallow = list("no levels"=TRUE),
      levels_prev = list(),
      num_sim_prev = NA,
      num_sim_cuml = 0,
      tid = NA,
      sim_var = "",
      update_sim = FALSE,
      env_calling = parent.frame()
    ),
    vars = list(
      seed = ..seed,
      env = environment(),
      num_sim_total = 10,
      run_state = "pre run"
    ),
    script = NULL,
    results = NULL,
    errors = NULL
  )

  # Create a global reference to the environment that can be searched for via
  #     get() by methods (currently only use_method) that need to access the
  #     simulation environment but don't take sim as an argument
  assign(x="..env", value=...sim$vars$env, envir=..e)
  rm(..e)

  class(...sim) <- "sim_obj"

  return (...sim)

}
