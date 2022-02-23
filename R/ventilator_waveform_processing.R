#' Generate volume waveform from flow
#' @param vent_df ventilation waveform data frame
#'
#' @export
add_volume_signal <- function(vent_df) {
    samp_interval <- 1000 / attributes(vent_df$flow)$signal.samplerate

    assertthat::assert_that(assertthat::are_equal(samp_interval, 20, tol = 0.01)) # 50 Hz

    # Calculate change in volume between two flow measurements
    # L/min -> ml/ms = 1000/60e3
    res <- dplyr::mutate(vent_df, .dVol = c(.data$flow[1], (.data$flow[1:length(.data$flow)-1] + .data$flow[2:length(.data$flow)])/2) *
                             (1000/60e3) * 20) # Trapezoid integration rule
    res <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(res, .data$insp), volume = cumsum(.data$.dVol)))
    dplyr::filter(res, .data$insp != 0)
}
