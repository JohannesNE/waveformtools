#' Calculate PPV using simple algorithm
#'
#' This function attempt to calculate PPV as described by De Backer, 2009.
#'
#'
#' @inheritParams find_PP_minmax
#' @param average_over_n Number of max-min pairs (resp cycles) to average over
#' @param average_fun Function used for averaging
#' @param plot_diagnostic Show diagnostic plot
#'
#' @return
#' @export
#'
calculate_classic_ppv <- function(beats, PP_col = "PP", time_col = 1,
                                  resp_rate = NULL, resp_len = NULL,
                                  average_over_n = 3, average_fun = mean,
                                  plot_diagnostic = FALSE) {

    df_minmax <- find_PP_minmax(beats, resp_rate = resp_rate, resp_len = resp_len,
                                PP_col = PP_col, time_col = time_col)

    df_minmax_selected <- dplyr::slice_tail(df_minmax, n = average_over_n)

    if(plot_diagnostic) {
        plot(beats[[time_col]], beats[[PP_col]], type = 'l',
             xlab = "time", ylab = "PP")
        with(df_minmax_selected, {
            points(time_max, PP_max, col = "red")
            points(time_min, PP_min, col = "green")
            })
    }

    average_fun(df_minmax_selected[["PPV"]])
}

#' Find respiratory min and max in pulse pressure time series
#'
#' @param beats Data frame of beats incl. pulse pressure
#' @param PP_col Column containing pulse pressure
#' @param time_col Column containing time (datetime or seconds)
#' @param resp_rate,resp_len Either resp_rate (breaths/min) or resp_len (seconds)
#'
#' @return
#' @export
#'
find_PP_minmax <- function(beats, PP_col = "PP", time_col = 1,
                           resp_rate = NULL, resp_len = NULL) {

    if (is.null(resp_len)) {
        if (is.null(resp_rate)) stop("Either resp_rate or resp_len must be supplied.")

        resp_len <- 60/resp_rate
    } else {
        if (!is.null(resp_rate)) warning("Only resp_len is used when both resp rate and resp_len is supplied.")
    }

    PP_df <- data.frame(time = beats[[time_col]],
                        PP = beats[[PP_col]])

    t_end <- PP_df$time[nrow(PP_df)]

    init_len <- round(nrow(PP_df) / 2)
    PP_max_vec <- numeric(init_len)
    PP_min_vec <- numeric(init_len)
    t_PP_min_vec <- rep(t_end, init_len)
    t_PP_max_vec <- rep(t_end, init_len)

    # Start half a resp length before, so the first search is only in 1 resp_len
    t_min <- PP_df$time[1]
    i <- 1
    while(t_min < t_end-resp_len*1.2) {
        ## df subset is slow. not a good solution here....
        # Potential beats to look in: next resp_len*1.5 seconds, not including current time (t_i)
        vent_window_max <- PP_df[PP_df$time > t_min & PP_df$time < t_min + resp_len,]

        # If the search frame is less than two beats, move one resp_len forward.
        if(nrow(vent_window_max) < 2) {
            t_min <- t_min + resp_len
            next
        }

        max_i <- which.max(vent_window_max$PP)
        PP_max <- vent_window_max$PP[max_i]
        t_max <- vent_window_max$time[max_i] # Update search start position

        # Find minimun
        vent_window_min <- PP_df[PP_df$time > t_max & PP_df$time < t_max + resp_len,]

        # If the search frame is less than two beats, move one resp_len forward.
        # Move t_min to start new search for min and max.
        if(nrow(vent_window_min) < 2) {
            t_min <- t_min + resp_len
            next
        }

        min_i <- which.min(vent_window_min$PP)
        PP_min <- vent_window_min$PP[min_i]
        t_min <- vent_window_min$time[min_i]

        # Populate vectors
        PP_max_vec[i] <- PP_max
        PP_min_vec[i] <- PP_min
        t_PP_max_vec[i] <- t_max
        t_PP_min_vec[i] <- t_min

        i <- i + 1
    }

    res <- data.frame(time_max = t_PP_max_vec[1:i-1],
               PP_max = PP_max_vec[1:i-1],
               time_min = t_PP_min_vec[1:i-1],
               PP_min = PP_min_vec[1:i-1]
    )

    res[["PPV"]] <- 100 * (res$PP_max - res$PP_min) / ((res$PP_max + res$PP_min) / 2)

    res
}

