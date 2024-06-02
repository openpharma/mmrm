stopifnot(any(grepl("simChef", search())))

# Custom new method which only kicks off the future jobs.
# This is modified from the fit() method, see
# https://github.com/Yu-Group/simChef/blob/main/R/experiment.R#L976C3-L980C62
Experiment$set(
  "public",
  "onlystart",
  function(n_reps = 1,
           future.globals = NULL,
           future.packages = NULL,
           future.seed = TRUE,
           verbose = 1, ...) {
    parallel_strategy <- "reps"

    dgp_list <- private$.get_obj_list("dgp")
    method_list <- private$.get_obj_list("method")

    if (length(dgp_list) == 0) {
      private$.throw_empty_list_error("dgp", "generate data from")
    }

    if (length(method_list) == 0) {
      private$.throw_empty_list_error("method", "fit methods in")
    }

    private$.update_fit_params()

    checkpoint <- FALSE
    n_reps_cached <- 0
    n_reps_total <- n_reps
    fit_results <- data.frame()

    if (verbose >= 1) {
      inform(sprintf("Fitting %s...", self$name))
      start_time <- Sys.time()
    }

    if (is.null(future.packages)) {
      future.packages <- private$.future.packages
    }

    if (is.null(future.globals)) {
      future.globals <- private$.future.globals
    }

    dgp_params_list <- private$.combine_vary_params("dgp")
    method_params_list <- private$.combine_vary_params("method")

    # if new_fit_params is not NULL after the if statement below, then not all
    # combos of (dgp_params_list, method_params_list) need to be rerun so need
    # to check cache ids when fitting
    new_fit_params <- NULL

    duplicate_param_names <- private$.get_duplicate_param_names()

    # simulations
    n_reps <- min(n_reps, n_reps_total - n_reps_cached)

    new_fit_results <- local({

      # create an env with objs/funcs that the future workers need
      workenv <- rlang::new_environment(
        data = list(
          verbose = verbose,
          dgp_list = dgp_list,
          method_list = method_list,
          new_fit_params = new_fit_params,
          dgp_params_list = dgp_params_list,
          method_params_list = method_params_list,
          duplicate_param_names = duplicate_param_names,
          do_call_wrapper = function(name,
                                     fun,
                                     params,
                                     verbose,
                                     call) {
            tryCatch(
              do_call_handler(
                name, fun, params, verbose, call
              ),
              error = identity
            )
          }
        ),
        parent = rlang::ns_env()
      )

      # get the experiment compute fun
      compute_fun <- compute_rep

      environment(compute_fun) <- workenv

      # compute the experiment
      compute_fun(n_reps,
                  future.globals,
                  future.packages,
                  future.seed)
    })

    gc()

    new_fit_results %>%
      dplyr::mutate(
        .rep = as.character(as.numeric(.rep) + n_reps_cached)
      ) %>%
      simplify_tibble()
  }
)
