# Functions to work with gam models

#' Get PPV from GAM model
#'
#' @param model Gam model
#' @param term term to calculate ppv from (e.g. 's(resp_index)')
#' @param ppv_only Return only PPV value. Otherwise return row of related parameters as well.
#'
#' @export
get_gam_ppv <- function(model, term, ppv_only = FALSE) {
  if (!("gam" %in% class(model))) {
    if (length(model) == 1 && is.na(model)) {
      min_est <- NA
      max_est <- NA
      min_se <- NA
      max_se <- NA
      PPV <- NA
      PPV_se <- NA
      model_intercept <- NA
    }
    else {
      stop("model is not a GAM model")
    }
  } else {
    smooth <- gratia::smooth_estimates(model, term)
    model_intercept <- unname(model$coefficients[1])
    term_variable <- stringr::str_replace(term, "s\\((.+)\\)", "\\1")

    max_i <- which.max(smooth$est)
    min_i <- which.min(smooth$est)
    max_est <- smooth$est[max_i]
    max_se <- smooth$se[max_i]
    min_est <- smooth$est[min_i]
    min_se <- smooth$se[min_i]

    PPV <- (max_est - min_est) / model_intercept

    # Standard error of difference https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1125071/

    PPV_se <- sqrt(max_se^2 + min_se^2) / model_intercept
  }

  if (ppv_only) {
    return(PPV)
  }

  dplyr::tibble(min_est,
    max_est,
    min_se,
    max_se,
    PPV,
    PPV_se,
    PPV_lower = PPV - 1.96 * PPV_se,
    PPV_upper = PPV + 1.96 * PPV_se,
    model_intercept,
    min_pos = smooth[[term_variable]][min_i],
    max_pos = smooth[[term_variable]][max_i]
  )
}

#' Create data frame of data, fit and residuals of pulse pressure GAM
#'
#' @param PP_gam GAM of PP
#'
#' @return data frame
#' @export
get_PP_gam_predictions <- function(PP_gam) {
  params <- purrr::map_chr(PP_gam$smooth, purrr::pluck, "label")

  dplyr::mutate(PP_gam$model,
    PP_predict = mgcv::predict.gam(PP_gam),
    PP_res = mgcv::residuals.gam(PP_gam),
    PP_insp = as.numeric(mgcv::predict.gam(PP_gam, type = "terms", terms = params[1])),
    PP_trend = as.numeric(mgcv::predict.gam(PP_gam, type = "terms", terms = params[2])),
    PP_detrend = PP_res + PP_insp,
    PP_mean = coef(PP_gam)[1]
  )
}

#' Plot pulse pressure GAM smooths and partial residuals
#'
#' @param PP_gam PP GAM
#' @param return_list If true, return list of plots instead of combined plot.
#' @param add.ci Show 95% confidence interval on plots.
#' @param add.intercept Add model intercept to trend plot.
#'
#' @return Patchworked ggplot
#' @export
plot_PP_gam <- function(PP_gam, return_list = FALSE, add.ci = TRUE, add.intercept = TRUE) {

  # Get smooth labels
  params <- purrr::map_chr(PP_gam$smooth, purrr::pluck, "label")
  terms <- purrr::map_chr(PP_gam$smooth, purrr::pluck, "term")



  beats_p <- get_PP_gam_predictions(PP_gam)

  # Model visualizations

  # Get representations of smooths
  newdata <- data.frame(resp = seq(0, 1, length.out = 100),
                        time = 0)
  names(newdata) <- terms

  insp_smooth <- gratia::smooth_estimates(PP_gam,
    smooth = params[1],
    data = newdata
  )

  intercept <- if(add.intercept) coef(PP_gam)[1] else 0

  time_smooth <- gratia::smooth_estimates(PP_gam,
                                         smooth = params[2]) |>
      mutate(est = est + intercept)


  geom_ci_ribbon <- function() {
      if (add.ci) {
          ggplot2::geom_ribbon(
              ggplot2::aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
              fill = ggplot2::alpha("black", 0),
              colour = "black",
              linetype = 2,
              outline.type = "both"
          )
      } else {
          NULL
      }
  }

  insp_smooth_plot <- ggplot2::ggplot(insp_smooth, ggplot2::aes_string(terms[1], "est")) +
    geom_ci_ribbon() +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(y = PP_detrend), data = beats_p, alpha = 0.7) +
    ggplot2::labs(
      x = "Index to inspiration start (relative)",
      y = "Additive effect on mean PP [mmHg]"
    ) +
    ggplot2::ggtitle("Position in respiratory cycle") +
    ggplot2::scale_x_continuous(labels = scales::percent)

  time_smooth_plot <- ggplot2::ggplot(time_smooth, ggplot2::aes_string(terms[2], "est")) +
    geom_ci_ribbon() +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Time [s]",
      y = "Additive effect on mean PP [mmHg]"
    ) +
    ggplot2::ggtitle("Trend over time")

  if (return_list) {
    list(insp_smooth_plot, time_smooth_plot)
  } else {
    if(requireNamespace('patchwork', quietly = TRUE)) {
      patchwork::wrap_plots(insp_smooth_plot, time_smooth_plot)
    } else {
      warning("returning list. Install {patchwork} to return combined plot")
      list(insp_smooth_plot, time_smooth_plot)
    }
  }

}

#' Plot model predictions and observations using Dygraphs
#'
#' @param ... One or more Gam models.
#' @param resid include residuals
#'
#' @export
dygraph_gam <- function(..., resid = FALSE) {
  gams <- rlang::list2(...)
  mod_names <- rlang::exprs(...)

  pred <- lapply(gams, stats::predict)
  resids <- NULL

  if (resid) {
    resids <- lapply(gams, resid, type = "response")
    names(resids) <- paste0(mod_names, "_resid")
  }

  names(pred) <- paste0(mod_names, "_pred")

  assertthat::assert_that(length(pred) == 1 || stats::sd(sapply(pred, length)) == 0) # All lengths equal

  df <- dplyr::bind_cols(
    time = gams[[1]]$model$time,
    obs = gams[[1]]$model[,1],
    pred,
    resids
  )

  dygraph_signal(df, dplyr::all_of(names(df)[-1]), main = '')
}

# Fit a GAM to the pulse pressure data, assuming a specific respiratory cycle length.
fit_PP_gam  <- function(resp_len, data) {
    PP_insp_indexed <- dplyr::mutate(data,
        insp_rel_index = (time_s %% resp_len) / resp_len)

    mgcv::gam(
    PP ~
    s(insp_rel_index,
      k = 15, # 15 knots.
      bs = "cc" # The basis is a cyclic cubic spline
      ) +
    s(time_s,
      bs = "cr" # The basis is a natural cubic spline.
      ),
    knots = list(insp_rel_index = c(0,1)),
    method = "REML",
    data = PP_insp_indexed)
}

# get reml from GAM
get_gam_reml  <- function(mod) {
    mod$gcv.ubre["REML"]
}

gam_resp_reml <- function(resp_len, data) {
    fit_PP_gam(resp_len, data) |>
        get_gam_reml()
}

#' Find resp length from Pulse Pressure data using GAM
#'
#' @param data Data frame of timings and pulse pressures of beats
#' @param PP_col Column with pulse pressure values
#' @param time_col Columns with times
#' @param search_interval Respiratory cycle length intervals to search
#'
#' @return The respiratory length that minimizes the GAM REML score
#' @export
#'
find_resp_len_from_PP <- function(data, PP_col = "PP",
                                  time_col = "time",
                                  search_interval = c(1.5, 7)) {
    data[["PP"]] <- data[[PP_col]]
    data[["time_s"]] <- as_seconds(data[[time_col]])
    opt_len <- optimize(gam_resp_reml, interval = search_interval, data=data)

    opt_len$minimum
}

#' Analyse Pulse Pressure data using a GAM
#' Respiratory cycle length is found by using the value that minimizes model
#' REML score
#'
#' @inheritParams find_resp_len_from_PP
#' @param plot Diagnostic GAM plot usign gratia::draw
#' @param return_GAM Return fitted GAM instead of data frame
#'
#' @return Data frame of respiratory cycle length, respiratory rate and PPV.
#' @export
#'
PP_gam_analysis <- function(data,
                            plot = FALSE,
                            return_GAM = FALSE,
                            PP_col = "PP",
                            time_col = "time",
                            search_interval = c(1.5, 7)) {

    data[["PP"]] <- data[[PP_col]]
    data[["time_s"]] <- as_seconds(data[[time_col]])
    opt_resp_len <- find_resp_len_from_PP(data=data, search_interval = search_interval)

    PP_gam <- fit_PP_gam(opt_resp_len, data)
    PPV <- get_gam_ppv(PP_gam, "s(insp_rel_index)", ppv_only = TRUE)

    if (plot) {
        gratia::draw(PP_gam, residuals = TRUE)
    }

    if (return_GAM) {
        return(PP_gam)
    }

    data.frame(resp_len = opt_resp_len, resp_rate = 60/opt_resp_len, PPV = PPV)

}
