#' Framework for running simulations on a cluster computing system
#'
#' @description This function allows for simulations to be run in parallel on a
#'     cluster computing system (CCS). See the \href{https://avi-kenny.github.io/SimEngine/articles/parallelization.html}{Parallelization}
#'     vignette for a detailed overview of how CCS parallelization works in
#'     \pkg{SimEngine}. \code{run_on_cluster} acts as a wrapper for the code in
#'     your simulation, organizing the code into three sections, labeled "first"
#'     (code that is run once at the start of the simulation), "main"
#'     (running the simulation script repeatedly), and "last" (code to process
#'     or summarize simulation results). This function is to be used in
#'     conjunction with job scheduler software (e.g., Slurm or Oracle Grid
#'     Engine) to divide the simulation into tasks that are run in parallel on
#'     the CCS. See the Parallelization documentation for a detailed overview of
#'     how CCS parallelization works in \pkg{SimEngine}.
#'     \code{\link{run}})), and "last" (usually code to process or summarize
#'     simulation results). This function interacts with cluster job scheduler
#'     software (e.g. Slurm or Oracle Grid Engine) to divide parallel tasks over
#'     cluster nodes.
#' @param first Code to run at the start of a simulation. This should be a block
#'     of code enclosed by curly braces {} that creates and sets up a simulation
#'     object.
#' @param main Code that will run for every simulation replicate. This should be
#'     a block of code enclosed by curly braces {}, and will typically be a
#'     single line of code calling the \code{\link{run}}) function. This code
#'     block will have access to the simulation object you created in the
#'     'first' code block, but any changes made here to the simulation object
#'     will not be saved.
#' @param last Code that will run after all simulation replicates have been run.
#'     This should be a block of code enclosed by curly braces {} that processes
#'     your simulation object (which at this point will contain your results),
#'     which may involve calls to \code{\link{summarize}}, creation of plots,
#'     and so on.
#' @param cluster_config A list of configuration options. You must specify
#'     either \code{js} (the job scheduler you are using) or \code{tid_var} (the
#'     name of the environment variable that your task ID is stored in); see
#'     examples. Run \code{js_support()} to see a list of job schedulers that
#'     are currently supported. You can optionally also specify \code{dir},
#'     which is a character string representing a path to a directory on the
#'     CCS; this directory will serve as your working directory and hold your
#'     simulation object and all temporary objects created by \pkg{SimEngine}.
#'     If unspecified, this defaults to the working directory of the R script
#'     that contains your simulation code).
#' @examples
#' \dontrun{
#' # The following code is saved in a file called my_simulation.R:
#' library(SimEngine)
#' run_on_cluster(
#'   first = {
#'     sim <- new_sim()
#'     create_data <- function(n) { return(rpois(n=n, lambda=20)) }
#'     est_lambda <- function(dat, type) {
#'       if (type=="M") { return(mean(dat)) }
#'       if (type=="V") { return(var(dat)) }
#'     }
#'     sim %<>% set_levels(estimator = c("M","V"), n = c(10,100,1000))
#'     sim %<>% set_script(function() {
#'       dat <- create_data(L$n)
#'       lambda_hat <- est_lambda(dat=dat, type=L$estimator)
#'       return(list("lambda_hat"=lambda_hat))
#'     })
#'     sim %<>% set_config(num_sim=100, n_cores=20)
#'   },
#'   main = {
#'     sim %<>% run()
#'   },
#'   last = {
#'     sim %>% summarize()
#'   },
#'   cluster_config = list(js="slurm")
#' )
#'
#' # The following code is saved in a file called run_sim.sh:
#' # #!/bin/bash
#' # Rscript my_simulation.R
#'
#' # The following lines of code are run on the CCS head node:
#' # sbatch --export=sim_run='first' run_sim.sh
#' # sbatch --export=sim_run='main' --array=1-20 --depend=afterok:101 run_sim.sh
#' # sbatch --export=sim_run='last' --depend=afterok:102 run_sim.sh
#' }
#' @export
run_on_cluster <- function(first, main, last, cluster_config) {

  cluster_execute(
    substitute(first),
    substitute(main),
    substitute(last),
    cluster_config
  )

}
