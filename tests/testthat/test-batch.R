
# Helper function to create/remove objects for tests
create_objs <- function(results) {

  r <- results
  get_val <- function(rep_id, mu, est) {
    r[which(r$rep_id==rep_id & r$n==10 & r$mu==mu & r$est==est), "mean_dat"]
  }

  return(c(
    get_val(rep_id=1, mu=2, est="H"),
    get_val(rep_id=1, mu=2, est="E"),
    get_val(rep_id=1, mu=2, est="Y"),
    get_val(rep_id=2, mu=2, est="H"),
    get_val(rep_id=2, mu=2, est="E"),
    get_val(rep_id=2, mu=2, est="Y"),
    get_val(rep_id=1, mu=5, est="H"),
    get_val(rep_id=1, mu=5, est="E"),
    get_val(rep_id=2, mu=5, est="H")
  ))

}

# Wrapper function to run simulations and tests
run_and_test <- function(which, parallel, n_cores) {

  # Set up and run simulation
  sim <- new_sim()
  create_data <- function(n, mu) { rnorm(n, mean=mu, sd=0.5) }
  sim %<>% set_levels(n=c(10,100), mu=c(2,5), est=c("H","E","Y"))
  if (is.na(n_cores)) {
    sim %<>% set_config(num_sim=3, batch_levels=c("n","mu"),
                        parallel=parallel)
  } else {
    sim %<>% set_config(num_sim=3, batch_levels=c("n","mu"),
                        parallel=parallel, n_cores=n_cores)
  }
  sim %<>% set_script(function() {
    batch({ dat <- create_data(L$n, L$mu) })
    return (list("mean_dat" = mean(dat)))
  })
  sim %<>% run()

  # # Debugging
  # sim <<- sim

  # Extract results and run tests
  obj_A <- create_objs(sim$results)
  test_that(paste("batch() operates correctly:", which), {
    expect_equal(obj_A[1], obj_A[2])
    expect_equal(obj_A[1], obj_A[3])
    expect_equal(obj_A[4], obj_A[5])
    expect_equal(obj_A[4], obj_A[6])
    expect_equal(obj_A[7], obj_A[8])
    expect_false(obj_A[1]==obj_A[4])
    expect_false(obj_A[1]==obj_A[7])
    expect_false(obj_A[7]==obj_A[9])
  })

}

# Wrapper function to run simulations and tests using run_on_cluster
run_and_test_cl <- function(which, cmplx=FALSE, n_cores, run_tests=T, ret=F,
                            err=F) {

  # Creating this function here to test scoping
  create_data <- function(n, mu) { rnorm(n, mean=mu, sd=0.5) }

  # Set up and run simulation
  run_on_cluster(
    first = {
      sim <- new_sim()
      sim %<>% set_levels(n=c(10,100), mu=c(2,5), est=c("H","E","Y"))
      if (is.na(n_cores)) {
        sim %<>% set_config(num_sim=3, batch_levels=c("n","mu"),
                            progress_bar=F)
      } else {
        sim %<>% set_config(num_sim=3, batch_levels=c("n","mu"),
                            progress_bar=F, n_cores=n_cores)
      }
      if (cmplx) {
        sim %<>% set_script(function() {
          batch({ dat <- create_data(L$n, L$mu) })
          if (err && L$est=="E") { stop("Error msg E") }
          return (list(
            "mean_dat" = mean(dat),
            .complex = list(a=1, "mean_dat2"=mean(dat))
          ))
        })
      } else {
        sim %<>% set_script(function() {
          batch({ dat <- create_data(L$n, L$mu) })
          if (err && L$est=="E") { stop("Error msg E") }
          return (list("mean_dat" = mean(dat)))
        })
      }
    },
    main = { sim %<>% run() },
    last = {},
    cluster_config = list(js="slurm")
  )

  # # Debugging
  # sim <<- sim

  # Extract results and run tests
  if (run_tests) {

    obj_A <- create_objs(sim$results)
    test_that(paste("batch() operates correctly:", which), {
      expect_equal(obj_A[1], obj_A[2])
      expect_equal(obj_A[1], obj_A[3])
      expect_equal(obj_A[4], obj_A[5])
      expect_equal(obj_A[4], obj_A[6])
      expect_equal(obj_A[7], obj_A[8])
      expect_false(obj_A[1]==obj_A[4])
      expect_false(obj_A[1]==obj_A[7])
      expect_false(obj_A[7]==obj_A[9])
    })

    if (cmplx) {

      uid_1 <- dplyr::filter(
        sim$results, n==10 & mu==2 & est=="H" & rep_id==1
      )$sim_uid
      uid_2 <- dplyr::filter(
        sim$results, n==10 & mu==2 & est=="E" & rep_id==1
      )$sim_uid
      uid_3 <- dplyr::filter(
        sim$results, n==10 & mu==2 & est=="H" & rep_id==2
      )$sim_uid
      test_that(paste("batch() operates correctly w complex data:", which), {
        expect_equal(get_complex(sim,uid_1)$mean_dat2,
                     get_complex(sim,uid_2)$mean_dat2)
        expect_false(
          get_complex(sim,uid_1)$mean_dat2==get_complex(sim,uid_3)$mean_dat2
        )
      })
    }

  }

  if (ret) { return(sim) }

}

# Make sure environment variables aren't set
Sys.setenv(SLURM_ARRAY_TASK_ID="")
Sys.setenv(sim_run="")

# Test set #1
set.seed(1)
run_and_test(which="01", parallel=FALSE, n_cores=NA)
# run_and_test(which="02", parallel="outer", n_cores=NA) # Causes error, since this may call >2 cores
run_and_test(which="03", parallel=TRUE, n_cores=1)
run_and_test(which="04", parallel=TRUE, n_cores=2)
# run_and_test(which="06", parallel="inner", n_cores=NA) # Causes error, since this may call >2 cores
# run_and_test(which="07", parallel="inner", n_cores=1) # Inner parallelization deprecated
# run_and_test(which="08", parallel="inner", n_cores=2) # Inner parallelization deprecated

# Test set #2
run_and_test_cl(which="09", cmplx=F, n_cores=NA)
run_and_test_cl(which="10", cmplx=F, n_cores=1)
run_and_test_cl(which="11", cmplx=F, n_cores=2)
run_and_test_cl(which="12", cmplx=T, n_cores=NA)
run_and_test_cl(which="13", cmplx=T, n_cores=1)
run_and_test_cl(which="14", cmplx=T, n_cores=2)

# Test set #3
Sys.setenv(sim_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
run_and_test_cl(which="", cmplx=F, n_cores=2, run_tests=F)
Sys.setenv(sim_run="main")
for (i in c(1:2)) {
  Sys.setenv(SLURM_ARRAY_TASK_ID=as.character(i))
  run_and_test_cl(which="", cmplx=F, n_cores=2, run_tests=F)
}
Sys.setenv(sim_run="last")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
run_and_test_cl(which="15", cmplx=F, n_cores=2, run_tests=T)

# Test set #4
for (j in c(2,3,6)) {
  Sys.setenv(sim_run="first")
  Sys.setenv(SLURM_ARRAY_TASK_ID="")
  run_and_test_cl(which="", cmplx=T, n_cores=j, run_tests=F)
  Sys.setenv(sim_run="main")
  for (i in c(1:j)) {
    Sys.setenv(SLURM_ARRAY_TASK_ID=as.character(i))
    run_and_test_cl(which="", cmplx=T, n_cores=2, run_tests=F)
  }
  Sys.setenv(sim_run="last")
  Sys.setenv(SLURM_ARRAY_TASK_ID="")
  run_and_test_cl(which="16", cmplx=F, n_cores=2, run_tests=T)
}

# Test set #5
Sys.setenv(sim_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
sim <- run_and_test_cl(which="", cmplx=F, n_cores=2, run_tests=F, ret=T, err=T)
Sys.setenv(sim_run="main")
for (i in c(1:2)) {
  Sys.setenv(SLURM_ARRAY_TASK_ID=as.character(i))
  sim <- run_and_test_cl(which="", cmplx=F, n_cores=2, run_tests=F, ret=T, err=T)
}
test_that("Errors handled correctly:", {
  expect_equal(file.exists("sim_results/r_01.rds"), TRUE)
  expect_equal(file.exists("sim_results/r_02.rds"), TRUE)
  expect_equal(file.exists("sim_results/e_01.rds"), TRUE)
  expect_equal(file.exists("sim_results/e_02.rds"), TRUE)
})
Sys.setenv(sim_run="last")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
sim <- run_and_test_cl(which="", cmplx=F, n_cores=2, run_tests=F, ret=T, err=T)
test_that("Errors handled correctly:", {
  expect_equal(nrow(sim$results), 24)
  expect_equal(nrow(sim$errors), 12)
  expect_equal(sim$errors[1,"message"], "Error msg E")
})

# Test set #6
Sys.setenv(sim_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
run_and_test_cl(which="", cmplx=F, n_cores=2, run_tests=F)
Sys.setenv(sim_run="main")
Sys.setenv(SLURM_ARRAY_TASK_ID="3")
test_that("Error handling: tid>n_cores", {
  expect_error(run_and_test_cl(which="", cmplx=F, n_cores=2, run_tests=F),
               "This simulation has n_cores=2, so this core will not be used.")
})

# Test set #7
Sys.setenv(sim_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
run_and_test_cl(which="", cmplx=F, n_cores=36, run_tests=F)
Sys.setenv(sim_run="main")
Sys.setenv(SLURM_ARRAY_TASK_ID="36")
test_that("Error handling: tid>num_batches", {
  expect_error(run_and_test_cl(which="", cmplx=F, n_cores=36, run_tests=F),
               paste0("This simulation only contains 12 replicate batches, so ",
                      "this core will not be used."))
})

# Test set #8
Sys.setenv(sim_run="first")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
sim <- run_and_test_cl(which="", cmplx=F, n_cores=NA, run_tests=F, ret=T)
Sys.setenv(sim_run="main")
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
sim <- run_and_test_cl(which="", cmplx=F, n_cores=NA, run_tests=F, ret=T)
Sys.setenv(SLURM_ARRAY_TASK_ID="2")
sim <- run_and_test_cl(which="", cmplx=F, n_cores=NA, run_tests=F, ret=T)
Sys.setenv(sim_run="last")
Sys.setenv(SLURM_ARRAY_TASK_ID="")
sim <- run_and_test_cl(which="", cmplx=F, n_cores=NA, run_tests=F, ret=T)
test_that("batch() throws an error if n_cores=NA:", {
  expect_equal(nrow(sim$errors), 2)
  expect_equal(sim$errors[1,"message"], paste0(
    "If the batch() function is used on a cluster computing system, you must s",
    "et the `n_cores` config option via set_config()"
  ))
})
rm(sim)

# Cleanup
Sys.setenv(SLURM_ARRAY_TASK_ID="")
Sys.setenv(sim_run="")
unlink("sim_results", recursive=T)
unlink("sim.rds")

# Test set #9
sim <- new_sim()
sim %<>% set_levels(n=c(10,100), mu=c(2,5))
sim %<>% set_config(num_sim=2)
sim %<>% set_script(function() {
  batch({ dat <- rnorm(1) })
  return (list("dat"=dat))
})
sim %<>% run()
test_that("batch() throws an error if batch_levels=NA:", {
  expect_equal(nrow(sim$errors), 8)
  expect_equal(sim$errors[1,"message"], paste0(
    "If the batch() function is used, you must set the `batch_levels` config o",
    "ption via set_config()"
  ))
})
rm(sim)

# Test set #10
sim <- new_sim()
sim %<>% set_levels(n=c(10,100), mu=c(2,5))
sim %<>% set_config(num_sim=2, batch_levels="n")
sim %<>% set_script(function() {
  batch({ dat <- rnorm(1) })
  return (list("dat"=dat))
})
sim %<>% run()
sim %<>% set_levels(n=c(10), mu=c(2,5))
sim %<>% update_sim()
test_that("Can remove reps from a simulation that uses batch()", {
  expect_equal(nrow(sim$results), 4)
  expect_equal(sim$errors, "No errors")
})
sim %<>% set_levels(n=c(10,1000), mu=c(2,5,8))
sim %<>% update_sim()
test_that("Cannot add reps to a simulation that uses batch()", {
  expect_equal(nrow(sim$errors), 8)
  expect_equal(sim$errors[1,"message"],
               paste0("You cannot add replicates to a simulation that uses the",
                      " batch() function"))
})
rm(sim)

# Test set #11
sim <- new_sim()
sim %<>% set_levels(alpha=c("A","B"), beta=c("C","D"))
sim %<>% set_config(num_sim=3, batch_levels=NULL)
sim %<>% set_script(function() {
  batch({ dat <- rnorm(n=10, mean=3) })
  mu_hat <- mean(dat)
  return(list("dat1"=round(dat[1],8)))
})
sim %<>% run()
r1 <- sim$results[which(sim$results$rep_id==1),"dat1"]
r2 <- sim$results[which(sim$results$rep_id==2),"dat1"]
test_that("batch() works with batch_levels=NULL", {
  expect_equal(r1[1], r1[2])
  expect_equal(r1[1], r1[3])
  expect_equal(r1[1], r1[4])
  expect_equal(r2[1], r2[2])
  expect_equal(r2[1], r2[3])
  expect_equal(r2[1], r2[4])
  expect_false(r1[1]==r2[1])
})
rm(sim,r1,r2)
