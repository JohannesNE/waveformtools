test_that("find PP minmax works", {
  samp_beats <- sample_record2$beats[50:100,]
  expect_error(find_PP_minmax(samp_beats))
  expect_warning(find_PP_minmax(samp_beats, resp_rate = 2, resp_len = 10))

  test_minmax <- find_PP_minmax(samp_beats, resp_len = 4.6)
  expect_equal(min(test_minmax$PP_min), 41.875)
  expect_equal(max(test_minmax$PP_max), 47.125)

  # ggplot(samp_beats, aes(time, PP)) +
  #     geom_point() +
  #     geom_line() +
  #     geom_point(aes(time_max, PP_max), data = test_minmax, color = "red") +
  #     geom_point(aes(time_min, PP_min), data = test_minmax, color = "green")
})

test_that("find PP minmax works with excluded beats", {
    samp_beats_excl <- sample_record2$beats[c(1:20, 30:50),]

    test_minmax <- find_PP_minmax(samp_beats_excl, resp_len = 4.6)
    expect_length(test_minmax$PP_min, 4)

    # ggplot(samp_beats_excl, aes(time, PP)) +
    #     geom_point() +
    #     geom_line() +
    #     geom_point(aes(time_max, PP_max), data = test_minmax, color = "red") +
    #     geom_point(aes(time_min, PP_min), data = test_minmax, color = "green")
})

high_rr_PP <- structure(list(time = structure(c(38439.6836709976, 38440.963670969,
                                  38442.2356710434, 38443.4996709824, 38444.7316710949, 38446.0196709633,
                                  38447.3156709671, 38448.5956709385, 38449.8676710129, 38451.1396710873,
                                  38452.4116709232, 38453.6916708946, 38454.9716711044, 38456.2436709404,
                                  38457.4836709499, 38458.8196709156, 38460.099670887, 38461.3636710644,
                                  38462.6596710682, 38463.9396710396, 38465.211671114, 38466.4916710854,
                                  38467.7796709538, 38469.0516710281), class = c("hms", "difftime"
                                  ), units = "secs"), PP = c(48.25, 47.875, 48.75, 49, 48.3125,
                                                             48.625, 48.5, 48.0625, 48.9375, 48.3125, 47.9375, 48.75, 48.0625,
                                                             47.875, 48.75, 48.0625, 47.9375, 48.6875, 47.8125, 48, 49.125,
                                                             47.625, 48.5, 48.5625)), row.names = c(NA, -24L), class = "data.frame")

test_that("find PP minmax works high resp rate", {
    test_minmax <- find_PP_minmax(high_rr_PP, resp_rate = 31)

    # ggplot(high_rr_PP, aes(time, PP)) +
    #     geom_point() +
    #     geom_line() +
    #     geom_point(aes(time_max, PP_max), data = test_minmax, shape = 3, color = "green") +
    #     geom_point(aes(time_min, PP_min), data = test_minmax, shape = 4, color = "red")

    expect_equal(test_minmax$PPV, c(0, 0, 1.41297366730893, 0.257400257400257, 0, 1.2853470437018,
                                   0, 1.42027114267269, 0, 1.42027114267269, 0, 1.81347150259067,
                                   0, 3.10077519379845, 0)
                 )
})

test_that("calculate_classic_ppv works", {
    samp_beats <- sample_record2$beats[50:100,]
    ppv <- calculate_classic_ppv(samp_beats, resp_len = 4.6)
    expect_equal(ppv, 8.211, tolerance = 0.01)
})
