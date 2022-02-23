#' Filter signal, defaluts to 2nd order Butterworth low pass filter
#'
#' @param signal Vector or signal dataframe with time column
#' @param cutoff_frequency Critical frequency of filter
#' @param signal_col column with signal to filter
#' @param sample_rate Sample rate of signal. If NULL, this is calculated from the time column.
#' @param type passed to [signal::butter()]. One of "low", "high", "stop", "pass"
#' @param trim_ends Seconds to trim off each end after filtering. To remove tails towards 0
#' @param postfix Postfix to the new column
#' @param filter_func Signal function to use for filtering
#' e.g. signal::cheby2
#' @param filter_order Passed to `filter_func` as `n`
#' @param ... Passed to `filter_func`
#'
#' @import utils
#' @import stats
#' @import graphics
#'
#' @export
filter_signal <- function(signal, cutoff_frequency, signal_col = 2,
                          sample_rate = NULL, type = 'low', trim_ends = 0,
                          postfix = '_filt', filter_func = signal::butter, filter_order = 2, ...) {
    if (is.data.frame(signal)) {
        signal_vec <- signal[[signal_col]]
        signal_name <- ifelse(is.numeric(signal_col), names(signal)[signal_col], signal_col)
        if (is.null(sample_rate)) {
            sample_rate <- 1 / mean(as.numeric(diff(head(signal$time))))
        }
    }
    # To set the half amplitude cutoff, divide with sample_rate/2
    # According to https://stackoverflow.com/a/49905340/9665680
    W <- cutoff_frequency/(sample_rate/2)

    filter_def <- do.call(filter_func, list(n = filter_order, W = W, type=type, ...))
    filt <- signal::filtfilt(filter_def, signal_vec)



    if (is.data.frame(signal)) {
        signal[[paste0(signal_name, postfix)]]  <- filt

        # Cut of tails towards 0
        signal[round(sample_rate * trim_ends):(length(filt)- round(sample_rate * trim_ends)), ]
    } else {

        # Cut of tails towards 0
        filt[round(sample_rate * trim_ends):(length(filt)- round(sample_rate * trim_ends))]
    }
}

#' Peak detection algorithm from https://github.com/stas-g/findPeaks
#'
#' @description To find valleys, use -x
#'
#' @param x signal vector
#' @param m Number of points of both sides of the peak with a lower value.
#' 2m = width of peak
#' @param na.ignore Ignore NA values in peak calculation
#' @export
find_peaks <- function (x, m = 3, na.ignore = FALSE){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
        z <- i - m + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < length(x), w, length(x))
        if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1], na.rm = na.ignore)) {
            return(i + 1)
        }  else {
            return(numeric(0))
        }
    })
    pks <- unlist(pks)
    pks
}
