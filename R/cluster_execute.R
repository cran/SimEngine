#' Framework for updating simulations on a cluster computing system
#'
#' @noRd
cluster_execute <- function(first,
                            main,
                            last,
                            cluster_config,
                            keep_errors = TRUE,
                            keep_extra = FALSE,
                            update_switch = FALSE) {

  # Capture current working directory and reset it on function exit
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  # error handle invalid options
  handle_errors(keep_errors, "is.boolean")
  handle_errors(keep_extra, "is.boolean")
  handle_errors(update_switch, "is.boolean")

  # Rename arguments to avoid potential naming conflicts with contents of
  #   first/main/last blocks
  ..first <- first
  ..main <- main
  ..last <- last
  ..cfg <- cluster_config
  rm(first)
  rm(main)
  rm(last)
  rm(cluster_config)

  # Run all code locally if simulation is not being run on cluster
  # !!!!! TO-DO make this work for update_switch = TRUE
  if (Sys.getenv("sim_run")=="") {

    # Run code locally (`first` block)
    eval(..first)

    # Extract the simulation object variable name
    ..env <- environment()
    ..count <- 0
    ..sim_var <- NA
    for (obj_name in ls(..env)) {
      if ("sim_obj" %in% class(get(x=obj_name, envir=..env))) {
        ..sim_var <- obj_name
        ..count <- ..count + 1
      }
    }
    if (is.na(..sim_var)) {
      stop("A simulation object must be created in the `first` block")
    }
    if (..count>1) {
      stop(paste("Multiple simulation objects were detected; only one may be",
                 "created in the `first` block"))
    }
    rm(..count)
    rm(..env)

    # Assign simulation object to ..sim_var in the parent environment
    assign(x=..sim_var, value=eval(as.name(..sim_var)), envir=parent.frame(n=2))

    # Run code locally (`main` and `last` blocks)
    eval(..main)
    assign(x=..sim_var, value=eval(as.name(..sim_var)), envir=parent.frame(n=2))
    eval(..last)
    assign(x=..sim_var, value=eval(as.name(..sim_var)), envir=parent.frame(n=2))

  } else {

    # Construct necessary paths
    ..path_sim_obj <- "sim.rds"
    ..path_sim_res <- "sim_results"

    # Error handling: incorrect Sys.getenv("run") variable
    if (!(Sys.getenv("sim_run") %in% c("first", "main", "last"))) {
      stop(paste("The 'sim_run' environment variable must equal either",
                 "'first', 'main', or 'last'."))
    }

  }

  # FIRST: Run 'first' code or return existing simulation object
  if (Sys.getenv("sim_run")=="first") {

    # Check that cfg$dir is a valid directory
    if (!is.null(..cfg$dir) && !dir.exists(..cfg$dir)) {
      stop(paste("Directory", ..cfg$dir, "does not exist."))
    }

    # Error handling: test to see that we can write to cfg$dir
    test_file <- paste0(..path_sim_obj, '.test')
    tryCatch(
      expr = { saveRDS(list(a=123,b=456), file=test_file) },
      error = function(e) {
        stop(paste0("Directory ", ..cfg$dir, " is not writable."))
      }
    )

    # Error handling: test to see that we can read from cfg$dir
    tryCatch(
      expr = { x <- readRDS(file=test_file) },
      error = function(e) {
        stop(paste0("Directory ", ..cfg$dir, " is not readable."))
      }
    )

    # Error handling: test to see that we can delete from cfg$dir
    tryCatch(
      expr = { unlink(test_file) },
      error = function(e) {
        stop(paste0("Files cannot be deleted from directory ", ..cfg$dir, "."))
      }
    )

    # Set working directory
    if (!is.null(..cfg$dir)) setwd(..cfg$dir)

    # Remove old files
    if (dir.exists(..path_sim_res)) { unlink(..path_sim_res, recursive=TRUE) }

    # Create directory to store simulation results
    dir.create(..path_sim_res)

    ..start_time <- Sys.time()

    # Run 'first' code
    eval(..first)

    # Extract the simulation object variable name
    ..env <- environment()
    ..count <- 0
    ..sim_var <- NA
    for (obj_name in ls(..env)) {
      if ("sim_obj" %in% class(get(x=obj_name, envir=..env))) {
        ..sim_var <- obj_name
        ..count <- ..count + 1
      }
    }
    if (is.na(..sim_var)) {
      if (update_switch) {
        stop("A simulation object must be read in the `first` block")
      } else {
        stop("A simulation object must be created in the `first` block")
      }

    }
    if (..count>1) {
      stop(paste("Multiple simulation objects were detected; only one may be",
                 "read in the `first` block"))
    }
    rm(..count)
    rm(..env)

    # Save simulation object
    ..sim <- eval(as.name(..sim_var))
    ..sim$internals$sim_var <- ..sim_var
    ..sim$vars$start_time <- ..start_time
    ..sim$config$parallel <- "cluster"
    saveRDS(..sim, file=..path_sim_obj)

  } else if (Sys.getenv("sim_run") %in% c("main","last")) {

    # Set working directory
    if (!is.null(..cfg$dir)) setwd(..cfg$dir)

    tryCatch(
      ..sim <- readRDS(..path_sim_obj),
      warning = function(w) {
        stop(paste(
          "Simulation object was not found. Make sure your 'first' function",
          "is not producing errors and returns a valid simulation object, and",
          "that your shell commands are correct and properly sequenced."))
      }
    )

    handle_errors(..sim, "is.sim_obj")

  }

  # MAIN: run simulation replicate and save results/errors
  if (Sys.getenv("sim_run")=="main") {

    # if there are error files in the results directory and stop_at_error==TRUE
    # skip this rep
    err_reps <- list.files(path = ..path_sim_res, pattern = "e_*")
    if (!(length(err_reps) > 0 & ..sim$config$stop_at_error)){

      # Error handling: tid_var and js
      if (is.null(..cfg$tid_var) && is.null(..cfg$js)) {
        stop("You must specify either 'js' or 'tid_var' in cluster_config.")
      }
      if (!is.null(..cfg$tid_var) && !is.null(..cfg$js)) {
        warning(paste0("Both 'js' and 'tid_var' were specified in cluster_conf",
                       "ig; js will be ignored."))
      }

      if (!is.null(..cfg$tid_var)) {
        tid_var <- ..cfg$tid_var
      } else if (!is.null(..cfg$js)) {

        # Make 'js' case insensitive
        ..cfg$js <- tolower(..cfg$js)

        if (!(..cfg$js %in% (js_support())$js_code)) {
          stop(paste("cluster_config variable 'js' is invalid; for a list of",
                     "supported job schedulers, run js_support()"))
        }

        tid_var <- dplyr::case_when(
          # !!!!! Create an internal R object that stores this info and also
          # stores the dataframe that js_support() currently manually parses
          ..cfg$js=="slurm" ~ "SLURM_ARRAY_TASK_ID",
          ..cfg$js=="ge" ~ "SGE_TASK_ID"
        )
      }

      tid <- as.numeric(Sys.getenv(tid_var))

      if (is.na(tid)) {
        stop("Task ID is missing.")
      }

      add_to_tid <- as.numeric(Sys.getenv("sim_add_to_tid"))
      if (!is.na(add_to_tid)) {
        tid <- tid + add_to_tid
      }

      if (tid<1 || tid>..sim$vars$num_sim_total) {
        stop(paste(
          "Task ID is invalid; must be an integer between 1 and",
          ..sim$vars$num_sim_total
        ))
      } else {
        if (update_switch) {
          # for updating, need to add number of previously run sims
          #     (num_sim_cuml)
          tid <- tid + ..sim$internals$num_sim_cuml
        }
        # Run 'main' code
        ..sim$internals$tid <- tid
        rm(tid)
        rm(add_to_tid)
        for (pkg in ..sim$config$packages) {
          do.call("library", list(pkg))
        }
        assign(..sim$internals$sim_var, ..sim)
        eval(..main)
        assign("..sim", eval(as.name(..sim$internals$sim_var)))
      }

      # Parse results filename and save
      fmt <- paste0("%0", nchar(..sim$vars$num_sim_total), "d")

      if (..sim$vars$run_state=="run, no errors") {
        saveRDS(
          list(
            "results" = ..sim$results,
            "results_complex" = ..sim$results_complex
          ),
          paste0(..path_sim_res, "/r_",
                 sprintf(fmt, ..sim$internals$tid), ".rds")
        )
      } else if (..sim$vars$run_state=="run, all errors") {
        saveRDS(
          ..sim$errors,
          paste0(..path_sim_res, "/e_",
                 sprintf(fmt, ..sim$internals$tid), ".rds")
        )
      }
      if (!is.character(..sim$warnings)) {
        saveRDS(
          ..sim$warnings,
          paste0(..path_sim_res, "/w_",
                 sprintf(fmt, ..sim$internals$tid), ".rds")
        )
      }
    }
  }

  # LAST: merge results/errors into simulation object, run 'last' code, and save
  if (Sys.getenv("sim_run")=="last") {

    # if there are error files in the results directory and stop_at_error==TRUE
    # skip this rep
    err_reps <- list.files(path = ..path_sim_res, pattern = "e_*")
    if (length(err_reps) > 0 & ..sim$config$stop_at_error){
      unlink(paste0(..path_sim_res, "/r_*"))
    } else {
      # Process result/error files
      files <- dir(paste0(..path_sim_res))
      results_df <- NULL
      results_complex <- list()
      errors_df <- NULL
      warnings_df <- NULL
      num_new <- 0
      for (file in files) {

        if (substr(file,1,1)=="r") {

          r <- readRDS(paste0(..path_sim_res, "/", file))

          if (is.null(results_df)) {
            results_df <- r$results
          } else {
            results_df[nrow(results_df)+1,] <- r$results
          }

          if (!is.na(r$results_complex)) {
            results_complex[[length(results_complex)+1]] <-
              r$results_complex[[1]]
          }

          num_new <- num_new + 1

        } else if (substr(file,1,1)=="e") {

          e <- readRDS(paste0(..path_sim_res, "/", file))

          if (class(e)=="data.frame") {
            if (is.null(errors_df)) {
              errors_df <- e
            } else {
              errors_df[nrow(errors_df)+1,] <- e
            }
          }

          num_new <- num_new + 1

        } else if (substr(file,1,1) == "w") {
          w <- readRDS(paste0(..path_sim_res, "/", file))

          if (class(w)=="data.frame") {
            if (is.null(warnings_df)) {
              warnings_df <- w
            } else {
              warnings_df[nrow(warnings_df)+1,] <- w
            }
          }
        }
      }

      if (identical(results_complex,list())) {
        results_complex <- NA
      }

      if (update_switch) {
        # combine results and errors with existing results and errors
        if (!is.character(..sim$results)) {
          results_df <- rbind(..sim$results, results_df)
          results_df <- results_df[order(results_df$sim_uid),]
        }
        if (!is.na(results_complex)) {
          results_complex <- c(..sim$results_complex, results_complex)
        }
        if (!is.character(..sim$errors)) {
          errors_df <- rbind(..sim$errors, errors_df)
          errors_df <- errors_df[order(errors_df$sim_uid),]
        }
        if (!is.character(..sim$warnings)) {
          warnings_df <- rbind(..sim$warnings, warnings_df)
          warnings_df <- warnings_df[order(warnings_df$sim_uid),]
        }
      }

      # Add results/errors to simulation object
      # Note: this code is somewhat redundant with the end of SimEngine::run
      if (!is.null(warnings_df)) {
        ..sim$warnings <- warnings_df
      }
      else {
        ..sim$warnings <- "No warnings"
      }
      if (!is.null(results_df) && !is.null(errors_df)) {
        ..sim$results <- results_df
        ..sim$results_complex <- results_complex
        ..sim$errors <- errors_df
        ..sim$vars$run_state <- "run, some errors"
      } else if (!is.null(results_df)) {
        ..sim$results <- results_df
        ..sim$results_complex <- results_complex
        ..sim$errors <- "No errors"
        ..sim$vars$run_state <- "run, no errors"
      } else if (!is.null(errors_df)) {
        ..sim$results <- "Errors detected in 100% of simulation replicates"
        ..sim$errors <- errors_df
        ..sim$vars$run_state <- "run, all errors"
      } else {
        stop("An unknown error occurred.")
      }

      levels_grid_big <- create_levels_grid_big(..sim)

      if (update_switch) {
        prev_levels_grid_big <- ..sim$internals$levels_grid_big

        # get levels / sim_ids that were previously run but are no longer needed
        extra_run <- dplyr::anti_join(
          prev_levels_grid_big[,-which(names(prev_levels_grid_big) %in%
                                         c("sim_uid", "level_id")),drop=F],
          levels_grid_big[,-which(names(levels_grid_big) %in%
                                    c("sim_uid", "level_id")),drop=F],
          by=names(prev_levels_grid_big[,-which(names(prev_levels_grid_big) %in%
                                                  c("sim_uid", "level_id")),
                                        drop=F])
        )

        # if keep_extra = FALSE, remove excess runs (from results, errors, and
        #     warnings)
        if (!keep_extra & nrow(extra_run) > 0){

          if (!is.character(..sim$results)){
            ..sim$results <- dplyr::anti_join(..sim$results,
                                                  extra_run,
                                                  by = names(extra_run))
          }
          if (!is.character(..sim$errors)){
            ..sim$errors <- dplyr::anti_join(..sim$errors,
                                                 extra_run,
                                                 by = names(extra_run))
          }
          if (!is.character(..sim$warnings)){
            ..sim$warnings <- dplyr::anti_join(..sim$warnings,
                                                   extra_run,
                                                   by = names(extra_run))
          }
        }
      }

      # record levels and num_sim that were run
      ..sim$internals$levels_prev <- ..sim$internals$levels_shallow
      ..sim$internals$num_sim_prev <- ..sim$config$num_sim
      ..sim$internals$levels_grid_big <- levels_grid_big

      if (update_switch) {
        ..sim$internals$update_sim <- TRUE
        ..sim$internals$num_sim_cuml <- ..sim$internals$num_sim_cuml + num_new
      } else {
        ..sim$internals$num_sim_cuml <- ..sim$internals$num_sim_cuml +
          ..sim$vars$num_sim_total
      }

      # Delete individual results files and save simulation object
      # This is done before running the 'last' code so that the compiled
      #   simulation object is saved even if there's an error with the 'last'
      #   code
      for (i in 1:5){
        x <- unlink(..path_sim_res, recursive=TRUE)
        if (x == 0){
          break
        }
        Sys.sleep(0.2)
      }
      saveRDS(..sim, file=..path_sim_obj)

      # Run 'last' code
      for (pkg in ..sim$config$packages) {
        do.call("library", list(pkg))
      }
      assign(..sim$internals$sim_var, ..sim)
      eval(..last)

      # Save final simulation object (a second time, if 'last' code had no errors)
      assign("..sim", eval(as.name(..sim$internals$sim_var)))
      ..sim$vars$end_time <- Sys.time()
      ..sim$vars$total_runtime <- as.numeric(
        difftime(..sim$vars$end_time, ..sim$vars$start_time),
        units = "secs"
      )
      saveRDS(..sim, file=..path_sim_obj)
    }

  }

}